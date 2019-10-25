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

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Scope = struct
  type t = Local | Global
                     [@@deriving show]

  let is_global = function
    | Global -> true
    | _ -> false

  let is_local x = not (is_global x)
end

type t =
  { datatype: Types.datatype;
    scope: Scope.t;
    name: string;
    ident: Ident.t;
    host: Ident.Persistent.t }
    [@@deriving show]

let make : ?datatype:Types.datatype -> ?scope:Scope.t -> Ident.Persistent.t -> Ident.t -> string -> t
  = fun ?(datatype=`Not_typed) ?(scope=Scope.Local) host ident name ->
  { datatype; scope; name; ident; host }

let equal : t -> t -> bool
  = fun x y ->
     Ident.equal x.ident y.ident
  && Ident.Persistent.equal x.host y.host

let compare x y =
  let result =
    Ident.Persistent.compare x.host y.host
  in
  if result = 0
  then Ident.compare x.ident y.ident
  else result

let origin : t -> Ident.Persistent.t
  = fun { host; _ } -> host

let modify : ?datatype:Types.datatype -> ?scope:Scope.t -> ?name:string -> t -> t
  = fun ?datatype ?scope ?name binder ->
  match datatype, scope, name with
  | None, None, None -> binder
  | Some datatype, Some scope, Some name ->
     { binder with datatype; scope; name }
  | Some datatype, Some scope, None ->
     { binder with datatype; scope }
  | Some datatype, None, Some name ->
     { binder with datatype; name }
  | Some datatype, None, None ->
     { binder with datatype }
  | None, Some scope, Some name ->
     { binder with scope; name }
  | None, Some scope, None ->
     { binder with scope }
  | None, None, Some name ->
     { binder with name }

let name : t -> string
  = fun { name; _ } -> name

let datatype : t -> Types.datatype
  = fun { datatype; _ } -> datatype

let to_ident : t -> Ident.t
  = fun { ident; _ } -> ident
