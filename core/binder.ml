open Utility

module Scope = struct
  type t = Local | Global
    [@@deriving show]

  let is_global : t -> bool = function
    | Global -> true
    | _      -> false

  let is_local : t -> bool
    = fun x -> not (is_global x)

  let default = Local
end

module Info = struct
  type t = { ty: Types.datatype
           ; scope: Scope.t
           ; name: string }
   [@@deriving show]

  let make : Scope.t -> Types.datatype -> string -> t
    = fun scope ty name ->
    { ty; name; scope }

  let of_type : Types.datatype -> t
    = fun ty -> make Scope.default ty ""

  let set_type : Types.datatype -> t -> t
    = fun ty info -> { info with ty }

  let set_scope : Scope.t -> t -> t
    = fun scope info -> { info with scope }

  let set_name : string -> t -> t
    = fun name info -> { info with name }
end

module Var = struct
  type t = int
    [@@deriving show,yojson]

  let counter = ref 0

  let dummy = (-1)

  let next : unit -> t
    = fun () -> incr counter; !counter

  let equal : t -> t -> bool
    = fun (x : int) y -> x = y

  let compare : t -> t -> int
    = fun (x : int) y ->
    if x > y then 1
    else if x < y then (-1)
    else 0
end

module Binder = struct
  type t = { info: Info.t
           ; var: Var.t }
   [@@deriving show]

  let of_info : Info.t -> Var.t -> t
    = fun info var ->
    { info; var }

  let make : Scope.t -> Types.datatype -> string -> Var.t -> t
    = fun scope ty name var ->
    of_info (Info.make scope ty name) var

  let fresh : ?scope:Scope.t -> ?ty:Types.datatype -> ?name:string -> unit -> t
    = fun ?(scope=Scope.default) ?(ty=Types.Not_typed) ?(name="") () ->
    make scope ty name (Var.next ())

  let datatype : t -> Types.datatype
    = fun { info; _ } -> info.Info.ty

  let set_datatype : Types.datatype -> t -> t
    = fun ty b -> { b with info = Info.set_type ty b.info }

  let var : t -> Var.t
    = fun b -> b.var

  let set_var : Var.t -> t -> t
    = fun var b -> { b with var }

  let info : t -> Info.t
    = fun { info; _ } -> info

  let name : t -> string
    = fun { info; _ } -> info.Info.name

  let set_name : string -> t -> t
    = fun name b -> { b with info = Info.set_name name b.info }

  let is_global : t -> bool
    = fun { info; _ } -> Scope.is_global info.Info.scope

  let is_local : t -> bool
    = fun b -> not (is_global b)

  let globalise : t -> t
   = fun b -> { b with info = Info.set_scope Scope.Global b.info }

  let localise : t -> t
    = fun b -> { b with info = Info.set_scope Scope.Local b.info }
end

include Binder

