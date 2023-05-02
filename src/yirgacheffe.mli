(**
  {1 Introduction}

  Yirgacheffe is a library for working with Geospatial data in the form
  of GeoTIFF files. This library is an OCaml implementation of the original
  {{: https://github.com/carbon-credits/yirgacheffe} python library} by
  Michael Dales.

  The main problem trying to be solved is memory-management and user experience.
  Often the calculations and algorithms performed over geospatial data is not
  too complicated, but ensuring you don't run out of memory and efficiently
  reading and writing the data can be somewhat involved. Yirgacheffe tries to
  remedy that.
*)

(** {2 Core Primitives} *)

module PixelScale : sig
  type t
  (** A pixel scale *)

  val x : t -> float
  (** The [x] value of the pixel scale. *)

  val y : t -> float
  (** The [y] value of the pixel scale. *)

  val create : x:float -> y:float -> t
  (** [create ~x ~y] makes a new pixel scale value from the [x]
      and [y] values. *)
end

module Area : sig
  type t
  (** A geometry with a top, bottom, left and right value. *)

  val create : left:float -> top:float -> right:float -> bottom:float -> t
end

module Window : sig
  type t
  (** A window *)

  val create : xoff:int -> yoff:int -> xsize:int -> ysize:int -> t
end

(** {2 Layers} *)
module type Low_layer = sig
    type t

    val of_file : Eio.File.ro -> t

    val area : t -> Area.t

    val pixel_scale : t -> PixelScale.t

    val window : t -> Window.t

    val set_area_of_interest : t -> Area.t -> t
end

module UniformArea : Low_layer

module Layer : sig
  type t
  val create : (module Low_layer with type t = 'a) -> 'a -> t
  val area : t -> Area.t
  val pixel_scale : t -> PixelScale.t
  val window : t -> Window.t
end

val calculate_intersection : Layer.t list -> Area.t