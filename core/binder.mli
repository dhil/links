module type S = sig
  module Scope: sig
    type t = Local | Global
             [@@deriving show]

    val is_global : t -> bool
    val is_local  : t -> bool
  end
  type t [@@deriving show]

  include Ident.IDENTIFIABLE with type t := t
  val origin : t -> Ident.Persistent.t
  val modify : ?datatype:Types.datatype -> ?scope:Scope.t -> ?name:string -> t -> t
  val name : t -> string
  val datatype : t -> Types.datatype
  val to_ident : t -> Ident.t
end

type t [@@deriving show]

include S with type t := t
val make : ?datatype:Types.datatype -> ?scope:Scope.t -> Ident.Persistent.t -> Ident.t -> string -> t
