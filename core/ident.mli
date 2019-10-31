open Utility
module type IDENTIFIABLE = sig
  type t [@@deriving show]
  val equal : t -> t -> bool
  val compare : t -> t -> int
end
include IDENTIFIABLE
val make : int -> t
val fresh : Gensym.t -> t
val to_string : t -> string

module Set : Set with type elt = t
module Map : Map with type key =t

module Persistent: sig
  include IDENTIFIABLE
  val of_string : string -> t
  val to_string : t -> string

  open Utility
  module Set : Set with type elt = t
  module Map : Map with type key = t
end

module type VAR = sig
  type root
  include IDENTIFIABLE
  val make : root -> Persistent.t list -> t
  val root : t -> root
  val path : t -> Persistent.t list
end

module Local: VAR with type root := t
module Remote: VAR with type root := Persistent.t
