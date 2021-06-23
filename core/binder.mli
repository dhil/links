module Scope: sig
  type t = Local | Global
    [@@deriving show]

  val is_global : t -> bool
  val is_local : t -> bool
end

module Info: sig
  type t
    [@@deriving show]
  val of_type : Types.datatype -> t
  val make : Scope.t -> Types.datatype -> string -> t
end

type t
  [@@deriving show]

module Var: sig
  type t = int
    [@@deriving show,yojson]

  val dummy : t
  val next : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

val of_info : Info.t -> Var.t -> t
val make : Scope.t -> Types.datatype -> string -> Var.t -> t
val fresh : ?scope:Scope.t -> ?ty:Types.datatype -> ?name:string -> unit -> t

val datatype : t -> Types.datatype
val set_datatype : Types.datatype -> t -> t

val info : t -> Info.t

val var : t -> Var.t
val set_var : Var.t -> t -> t

val name : t -> string
val set_name : string -> t -> t

val is_global : t -> bool
val is_local : t -> bool

val globalise : t -> t
val localise : t -> t
