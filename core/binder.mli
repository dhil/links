module type S = sig
  module Scope: sig
    type t = Local | Global
             [@@deriving show]

    val is_global : t -> bool
    val is_local  : t -> bool
  end
  type t

  include Ident.IDENTIFIABLE with type t := t
  val origin : t -> Ident.Persistent.t
  val modify : ?datatype:Types.datatype -> ?scope:Scope.t -> ?name:string -> t -> t
  val name : t -> string
  val datatype : t -> Types.datatype
  val to_ident : t -> Ident.t

  (* Using [@@deriving show] on `t` generates a couple of warnings
     about `pp` and `show`. Listing them explicitly silences the
     warnings. *)
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

include S
val make : ?datatype:Types.datatype -> ?scope:Scope.t -> Ident.Persistent.t -> Ident.t -> string -> t
