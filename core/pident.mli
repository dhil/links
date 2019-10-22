type t
   [@@deriving show]

val of_name : string -> t
val equal : t -> t -> bool
val compare : t -> t -> int
