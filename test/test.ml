open Yirgacheffe

let or_raise = function
  | Ok v -> v
  | Error _ -> failwith "Something's gone wrong!"

let gdal_dataset_of_region ~sw ?filename ~pixel_pitch (area : Area.t) =
  let driver, filename = match filename with
    | None -> Gdal.Driver.get_by_name "mem", "mem"
    | Some fname -> Gdal.Driver.get_by_name "GTiff", fname
  in
  let driver = or_raise driver in
  let xsize = Math.round_up_pixels ~scale:pixel_pitch ((area.right -. area.left) /. pixel_pitch) in
  let ysize = Math.round_up_pixels ~scale:pixel_pitch ((area.top -. area.bottom) /. pixel_pitch) in
  let ds = Gdal.Driver.create
    ~sw
    ~filename
    ~xsize
    ~ysize
    ~bands:1
    ~typ:`Byte
    driver
  in
  Gdal.Dataset.set_geo_transform ds [area.left; pixel_pitch; 0.0; area.top; 0.0; pixel_pitch *. -1.0] |> or_raise;
  Gdal.Dataset.set_projection ds wsg_84_projection |> or_raise;
  let band = Gdal.Dataset.raster_band ds 1 |> Option.get in
  let ysize = Gdal.Dataset.raster_y_size ds in
  let x_size = Gdal.Dataset.raster_x_size ds in
  for yoffet = 0 to ysize do
    let arr = Bigarray.Array1.(create Float64) C_layout x_size in
    for x = 0 to x_size - 1 do
      Bigarray.Array1.set arr x (float_of_int @@ yoffet mod 256)
    done;
    Gdal.RasterBand.write ~x:0 ~y:yoffet band arr |> or_raise
  done;
  ds

let area = Alcotest.testable Area.pp Area.equal

let find_single_intersection_item () =
  Eio.Switch.run @@ fun sw ->
  let ds = gdal_dataset_of_region ~sw (Area.create ~left:(-10.) ~right:10. ~top:10. ~bottom:(-10.)) ~pixel_pitch:0.02 in
  let raster_layer = Raster.of_dataset ds in
  let layer = Layer.create (module Raster) raster_layer in
  let inter = Yirgacheffe.calculate_intersection [ layer ] in
  Alcotest.check area "same intersection" (Layer.area layer) inter

let find_intersection_same () =
  Eio.Switch.run @@ fun sw ->
  let datasets = [
    gdal_dataset_of_region ~sw (Area.create ~left:(-10.) ~right:10. ~top:10. ~bottom:(-10.)) ~pixel_pitch:0.02;
    gdal_dataset_of_region ~sw (Area.create ~left:(-10.) ~right:10. ~top:10. ~bottom:(-10.)) ~pixel_pitch:0.02
  ] in
  let layers = List.map (fun f -> Layer.create (module Raster) (Raster.of_dataset f)) datasets in
  let inter = Yirgacheffe.calculate_intersection layers in
  Alcotest.check area "same area" (Layer.area @@ List.hd layers) inter

let find_intersection_subset () =
  Eio.Switch.run @@ fun sw ->
  let datasets = [
    gdal_dataset_of_region ~sw (Area.create ~left:(-10.) ~right:10. ~top:10. ~bottom:(-10.)) ~pixel_pitch:0.02;
    gdal_dataset_of_region ~sw (Area.create ~left:(-1.) ~right:1. ~top:1. ~bottom:(-1.)) ~pixel_pitch:0.02
  ] in
  let layers = List.map (fun f -> Layer.create (module Raster) (Raster.of_dataset f)) datasets in
  let inter = Yirgacheffe.calculate_intersection layers in
  Alcotest.check area "same area" (Layer.area @@ List.hd layers) inter

let () =
  Eio_main.run @@ fun _ ->
  Alcotest.run "yirgacheffe" [
    "intersection", [
      "find", `Quick, find_single_intersection_item;
      "same", `Quick, find_intersection_same;
      "sub", `Quick, find_intersection_subset;
    ]
  ]