let wsg_84_projection = {|GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AXIS["Latitude",NORTH],AXIS["Longitude",EAST],AUTHORITY["EPSG","4326"]]|}

module Math = struct
  let minimal_degree_of_interest = 1.0

  let round_up_pixels ~scale v =
    let floored = floor v in
    let diff = v -. floored in
    let degrees_diff = diff *. scale in
    if degrees_diff < minimal_degree_of_interest
    then int_of_float floored
    else int_of_float @@ ceil v
end

module PixelScale = struct
  type t = { x : float; y : float }

  let pp ppf v = Fmt.(list ~sep:Fmt.comma float) ppf [ v.x; v.y ]

  let x t = t.x
  let y t = t.y

  let create ~x ~y = { x; y }
end

module Area = struct
  type t = {
    left : float;
    right : float;
    top : float;
    bottom : float;
  }

  let equal a b =
    Float.equal a.left b.left &&
    Float.equal a.right b.right &&
    Float.equal a.top b.top &&
    Float.equal a.bottom b.bottom

  let pp ppf v = Fmt.(list float) ppf [ v.left; v.right; v.top; v.bottom ]

  let create ~left ~top ~right ~bottom = { left; top; right; bottom }
end

module Window = struct
  type t = {
    xoff : int;
    yoff : int;
    xsize : int;
    ysize : int;
  }

  let create ~xoff ~yoff ~xsize ~ysize =
    { xoff; yoff; xsize; ysize }
end

module type Low_layer = sig
  type t

  val area : t -> Area.t

  val pixel_scale : t -> PixelScale.t

  val window : t -> Window.t

  val set_area_of_interest : t -> Area.t -> t

  (* val with_data_at : t -> Window.t -> t * int -> unit *)
end

module UniformArea = struct
  type t = {
    tiff : Eio.File.ro Tiff.t;
    interest : Area.t option;

    projection : string;
    pixel_scale : PixelScale.t;
    geo_transform : float array;
    area : Area.t;
    window : Window.t;
  }

  let pixel_scale t = t.pixel_scale

  let set_area_of_interest t a = { t with interest = Some a }

  let area t =
    let height = Tiff.Ifd.height (Tiff.ifd t.tiff) in
    Area.create
      ~left:(-180.0)
      ~top:(t.geo_transform.(3))
      ~right:180.0
      ~bottom:(t.geo_transform.(3) +. (float_of_int height *. t.pixel_scale.y))

  let window t =
    let height = Tiff.Ifd.height (Tiff.ifd t.tiff) in
    Window.create
      ~xoff:0
      ~yoff:0
      ~xsize:(int_of_float (360.0 /. t.pixel_scale.x))
      ~ysize:height

  let of_file path =
    let tiff = Tiff.from_file path in
    let ifd = Tiff.ifd tiff in
    assert (Tiff.Ifd.width ifd = 1);
    let height = Tiff.Ifd.height ifd in
    let raw_pixel_scale = Tiff.Ifd.pixel_scale ifd in
    let pixel_scale =
      match raw_pixel_scale with
      | x :: y :: _ -> PixelScale.create ~x ~y:(Float.neg y)
      | _ -> assert false
    in
    let a, b =
      match Tiff.Ifd.tiepoint ifd with
      | _ :: _ :: _ :: y :: z :: _  :: [] -> y, z
      | _ -> assert false
    in
    let geo_transform = [| a; pixel_scale.x; 0.0; b; 0.0; pixel_scale.y |] in
    let projection = Tiff.Ifd.geo_ascii_params ifd in
    let area =
      Area.create
       ~left:(-180.0)
       ~top:geo_transform.(3)
       ~right:180.0
       ~bottom:(geo_transform.(3) +. (float_of_int height *. pixel_scale.y))
    in
    let window =
      Window.create
        ~xoff:0
        ~yoff:0
        ~xsize:(int_of_float @@ 360.0 /. pixel_scale.x)
        ~ysize:height
    in
    {
      tiff;
      interest = None;
      projection;
      pixel_scale;
      geo_transform;
      area;
      window;
    }
end

module Raster = struct
  type t = {
    dataset : Gdal.Dataset.t;
    transform : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
    raster_x_size : int;
    raster_y_size : int;
    interest : Area.t option;
    area : Area.t;
    window : Window.t;
  }

  let ( >>= ) f v = Result.bind f v

  let gdal_error = function
    | Ok v -> Ok v
    | Error e -> Error (Gdal.Error.to_msg e)

  let of_dataset dataset =
    let transform = Gdal.Dataset.geo_transform dataset |> Result.get_ok in
    let raster_x_size = Gdal.Dataset.raster_x_size dataset in
    let raster_y_size = Gdal.Dataset.raster_y_size dataset in
    let area =
      Area.create
        ~left:(Bigarray.Array1.get transform 0)
        ~top:(Bigarray.Array1.get transform 3)
        ~right:(Bigarray.Array1.get transform 0 +. (float_of_int raster_x_size *. Bigarray.Array1.get transform 1))
        ~bottom:(Bigarray.Array1.get transform 3 +. (float_of_int raster_y_size *. Bigarray.Array1.get transform 5))
    in
    let window =
      Window.create
       ~xoff:0
       ~yoff:0
       ~xsize:raster_x_size
       ~ysize:raster_y_size
    in
    {
      dataset;
      transform;
      raster_x_size;
      raster_y_size;
      interest = None;
      area;
      window;
    }


  let _empty ?file ~sw ~projection (area : Area.t) (scale : PixelScale.t) (datatype : Gdal.Datatype.t) =
    let abs_xstep, abs_ystep =
      abs_float scale.x, abs_float scale.y
    in
    let pixel_friendly_area =
      Area.create
        ~left:(floor(area.left /. abs_xstep) *. abs_xstep)
        ~right:(floor(area.right /. abs_xstep) *. abs_xstep)
        ~top:(floor(area.top /. abs_ystep) *. abs_ystep)
        ~bottom:(floor(area.bottom /. abs_ystep) *. abs_ystep)
    in
    let driver, filename =
      match file with
      | None -> Gdal.Driver.get_by_name "mem", ""
      | Some name -> Gdal.Driver.get_by_name "GTiff", name
    in
    driver >>= fun driver ->
    let xsize = Math.round_up_pixels ~scale:abs_xstep (pixel_friendly_area.right -. pixel_friendly_area.left) in
    let ysize = Math.round_up_pixels ~scale:abs_ystep (pixel_friendly_area.top -. pixel_friendly_area.bottom) in
    let dataset = Gdal.Driver.create ~sw ~filename ~xsize ~ysize ~bands:1 ~typ:datatype driver in
    Gdal.Dataset.set_geo_transform dataset [pixel_friendly_area.left; scale.x; 0.0; pixel_friendly_area.top; 0.0; scale.y] |> gdal_error >>= fun () ->
    Gdal.Dataset.set_projection dataset projection |> gdal_error

  let pixel_scale t =
    PixelScale.create ~x:(Bigarray.Array1.get t.transform 1) ~y:(Bigarray.Array1.get t.transform 5)

  let set_area_of_interest t a = { t with interest = Some a }

  let area t = t.area

  let window t = t.window
end


(* Generic Layers *)
module Layer = struct
  type t = Layer : ((module Low_layer with type t = 'a) * 'a) -> t

  let create (type a) (module L : Low_layer with type t = a) (v : a) = Layer ((module L), v)

  let area (Layer ((module L), v)) = L.area v
  let pixel_scale (Layer ((module L), v)) = L.pixel_scale v
  let window (Layer ((module L), v)) = L.window v
end

let almost_equal f1 f2 =
  Float.abs (f1 -. f2) < Float.epsilon

let max_float_list =
  List.fold_left (fun acc v -> if Float.compare acc v > 0 then acc else v) (-1_000_000_000_000.)

let min_float_list =
  List.fold_left (fun acc v -> if Float.compare acc v > 0 then v else acc) Float.max_float

let calculate_intersection (layers : Layer.t list) = match layers with
  | [] -> failwith "No layers provided"
  | first :: _ ->
    let scale = Layer.pixel_scale first in
    let same_scale l2 =
      let p2 = Layer.pixel_scale l2 in
      Fmt.pr "P1: %a\n" PixelScale.pp scale;
      Fmt.pr "P2: %a\n" PixelScale.pp p2;
      almost_equal scale.x p2.x && almost_equal scale.y p2.y
    in
    let are_layers_same_scale = List.for_all same_scale layers in
    if not are_layers_same_scale then failwith "Layers are not same scale"
    else begin
      let intersection =
        Area.create
        ~left:(List.map (fun v -> (Layer.area v).left) layers |> max_float_list)
        ~right:(List.map (fun v -> (Layer.area v).right) layers |> min_float_list)
        ~bottom:(List.map (fun v -> (Layer.area v).bottom) layers |> max_float_list)
        ~top:(List.map (fun v -> (Layer.area v).top) layers |> min_float_list)
      in
      if intersection.left < intersection.right && intersection.bottom < intersection.top then
        failwith "No intersetion possible"
      else
        intersection
    end