module type IDENTIFIABLE = sig
  type t [@@deriving show]
  val equal : t -> t -> bool
  val compare : t -> t -> int
end
type t = int [@@deriving show]
let make x = x
let equal : t -> t -> bool = (=)
let compare : t -> t -> int = Stdlib.compare

(* Binders live in compilation units. *)
module Binder = struct
  module type S = sig
    module Scope: sig
      type t = Local | Global
               [@@deriving show]

      val is_global : t -> bool
      val is_local  : t -> bool
    end
    type ident = t
    type t

    include IDENTIFIABLE with type t := t
    val origin : t -> Pident.t
    val modify : ?datatype:Types.datatype -> ?scope:Scope.t -> ?name:string -> t -> t
    val name : t -> string
    val datatype : t -> Types.datatype
    val to_ident : t -> ident
  end

  module Scope = struct
    type t = Local | Global
             [@@deriving show]

    let is_global = function
      | Global -> true
      | _ -> false

    let is_local x = not (is_global x)
  end

  type ident = t
  type t =
    { datatype: Types.datatype;
      scope: Scope.t;
      name: string;
      ident: int; (* Var *)
      host: Pident.t }
      [@@deriving show]

  let make : ?datatype:Types.datatype -> ?scope:Scope.t -> Pident.t -> ident -> string -> t
    = fun ?(datatype=`Not_typed) ?(scope=Scope.Local) host ident name ->
    { datatype; scope; name; ident; host }

  let equal : t -> t -> bool
    = fun x y ->
    equal x.ident y.ident && Pident.equal x.host y.host

  let compare x y =
    let result = Pident.compare x.host y.host in
    if result = 0
    then compare x.ident y.ident
    else result

  let origin : t -> Pident.t
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

  let to_ident : t -> ident
    = fun { ident; _ } -> ident
end

(* Compilation unit names, interface names. *)
module Persistent = Pident

module Make(I : sig
             type t

             val equal : t -> t -> bool
             val compare : t -> t -> int
             (* Required for pretty-printing. *)
             val pp : Format.formatter -> t -> unit
           end) = struct
  type t = { root: I.t;
             path: Persistent.t list (* non-empty *) }
           [@@deriving show]

  let make : I.t -> Persistent.t list -> t
    = fun root path -> { root; path }

  let equal : t -> t -> bool
    = fun x y ->
    let rec equal xs ys =
      match xs, ys with
      | [], [] -> true
      | x :: xs', y :: ys' ->
         Persistent.equal x y && equal xs' ys'
      | _, _ -> false
    in
    I.equal x.root y.root && equal x.path y.path

  let compare : t -> t -> int =
    fun x y ->
    let rec compare xs ys =
      match xs, ys with
      | [], [] -> 0
      | x :: xs', y :: ys' ->
         let result = Persistent.compare x y in
         if result = 0
         then compare xs' ys'
         else result
      | _x :: _, [] -> 1
      | [], _y :: _ -> (-1)
    in
    let result = I.compare x.root y.root in
    if result = 0
    then compare x.path y.path
    else result
end


(* Compilation unit local names. *)
module Local = Make(struct
                   type t = int [@@deriving show]
                   (* The type annotations are necessary to tell the
                      compiler to pick the fast comparison functions
                      for integers. *)
                   let equal : t -> t -> bool = (=)
                   let compare : t -> t -> int = Stdlib.compare
                 end)

(* Names originating from a foreign compilation unit. *)
module Remote = Make(struct
                    type t = Persistent.t [@@deriving show]
                    let equal = Persistent.equal
                    let compare = Persistent.compare
                  end)
