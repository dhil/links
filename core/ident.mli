module type IDENTIFIABLE = sig
  type t [@@deriving show]
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Persistent = Pident

module Binder: sig
  module type S = sig
    module Scope: sig
      type t = Local | Global
                         [@@deriving show]

      val is_global : t -> bool
      val is_local  : t -> bool
    end
    type t [@@deriving show]

    include IDENTIFIABLE with type t := t
    val origin : t -> Pident.t
    val modify : ?datatype:Types.datatype -> ?scope:Scope.t -> ?name:string -> t -> t
    val name : t -> string
    val datatype : t -> Types.datatype
  end

  type t [@@deriving show]

  include S with type t := t
  val make : ?datatype:Types.datatype -> ?scope:Scope.t -> Persistent.t -> int -> string -> t
end

module Local: sig
  include IDENTIFIABLE
  val make : int -> Persistent.t list -> t
end

module Remote: sig
  include IDENTIFIABLE
  val make : Persistent.t -> Persistent.t list -> t
end
