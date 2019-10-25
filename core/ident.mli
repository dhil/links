module type IDENTIFIABLE = sig
  type t [@@deriving show]
  val equal : t -> t -> bool
  val compare : t -> t -> int
end
include IDENTIFIABLE
val make : int -> t

module Persistent: sig
  include IDENTIFIABLE
  val of_name : string -> t

  open Utility
  module Set : Set with type elt = t
  module Map : Map with type key = t
end

module Local: sig
  include IDENTIFIABLE
  val make : int -> Persistent.t list -> t
end

module Remote: sig
  include IDENTIFIABLE
  val make : Persistent.t -> Persistent.t list -> t
end
