module PixelScale = struct
  type t = { x : float; y : float }

  let x t = t.x
  let y t = t.y

  let create ~x ~y = { x; y }
end

module Area = struct
  type t = {
    left : float;
    top : float;
    bottom : float;
    right : float;
  }

  let create ~left ~top ~right ~bottom =
    if left >= right || bottom >= top then
      invalid_arg "Invalid Area Dimensions"
    else
      { left; top; right; bottom }
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

  val of_file : Eio.File.ro -> t

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