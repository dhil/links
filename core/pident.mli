open Utility

type t [@@deriving show]

val of_name : string -> t
val equal : t -> t -> bool
val compare : t -> t -> int

module Set : Set with type elt = t
module Map : Map with type key = t
