(* Graph-based intermediate representation for JavaScript code. *)

(* Pointer abstraction *)
module Pointer: sig
  type 'a t

  (* Creation and modification. *)
  val null : unit -> 'a t
  val ref : 'a -> 'a t
  val set : 'a t -> 'a -> unit

  (* Inspection. *)
  val deref : 'a t -> 'a option
  val (!) : 'a t -> 'a option
  val is_null : 'a t -> bool

  (* A collection of *unsafe* operations. *)
  module Unsafe: sig
    exception NullPointerException
    val deref : 'a t -> 'a
  end

end = struct
  type 'a t = 'a option ref

  let null : unit -> 'a t
    = fun () -> ref None

  let ref : 'a -> 'a t
    = fun x -> ref (Some x)

  let deref : 'a t -> 'a option
    = fun p -> !p

  let (!) : 'a t -> 'a option
    = fun p -> !p

  let set : 'a t -> 'a -> unit
    = fun p x -> p := Some x

  let is_null p
    = match !p with
    | None -> true
    | _    -> false

  module Unsafe = struct
    exception NullPointerException

    let deref p =
      match !p with
      | None -> raise NullPointerException
      | Some p -> p
  end
end

(* Circular doubly linked list. *)
module DoublyList: sig
  type 'a t

  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val sonc : 'a t -> 'a -> 'a t

  val value  : 'a t -> 'a
  val forward : 'a t -> 'a t
  val backward : 'a t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length : 'a t -> int
  val remove : 'a t -> ('a * 'a t option)
  val set    : 'a t -> 'a -> unit
end = struct
  type 'a t = {
      mutable prev: 'a t option;
      mutable data: 'a;
      mutable next: 'a t option
    }

  let from_some : 'a option -> 'a = function
    | Some x -> x
    | _ -> assert false

  let singleton : 'a -> 'a t
    = fun x ->
    let xs =
      { prev = None;
        data = x;
        next = None; }
    in
    xs.prev <- Some xs;
    xs.next <- Some xs;
    xs

  let cons : 'a -> 'a t -> 'a t
    = fun x xs ->
    let ys = from_some xs.prev in
    let zs = singleton x in
    ys.next <- Some zs;
    zs.prev <- Some ys;
    zs.next <- Some xs;
    xs.prev <- Some zs;
    zs

  let sonc : 'a t -> 'a -> 'a t
    = fun xs x ->
    let ys = from_some xs.next in
    let zs = singleton x in
    ys.prev <- Some zs;
    zs.next <- Some ys;
    xs.next <- Some zs;
    zs.prev <- Some xs;
    zs

  let fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    = fun f z xs ->
    let rec foldl f z start xs =
      let acc = f z xs.data in
      let next = from_some xs.next in
      if start == next then acc
      else foldl f acc start next
    in
    foldl f z xs xs

  let fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    = fun f xs z ->
    let rec foldr f z start xs =
      let acc = f xs.data z in
      let prev = from_some xs.prev in
      if start == prev then acc
      else foldr f acc start prev
    in
    foldr f z xs xs

  let iter : ('a -> unit) -> 'a t -> unit
    = fun f xs ->
    let rec iter f start xs =
      f xs.data;
      match xs.next with
      | Some ys when not (ys == start) ->
         iter f start ys
      | _ -> ()
    in
    iter f xs xs

  let length : 'a t -> int
    = fun xs -> fold_left (fun acc _ -> acc + 1) 0 xs

  let remove : 'a t -> ('a * 'a t option)
    = fun xs ->
    if length xs = 1 then
      (xs.data, None)
    else
      let ys = from_some xs.prev in
      let zs = from_some xs.next in
      xs.next <- Some xs;
      xs.prev <- Some xs;
      ys.next <- Some zs;
      zs.prev <- Some ys;
      (xs.data, Some zs)

  let value xs = xs.data

  let forward xs = from_some xs.next
  let backward xs = from_some xs.prev

  let set xs x =
    xs.data <- x
end

(* Graphical Intermediate Representation for JavaScript programs. *)
type 'a ptr = 'a Pointer.t
type reference_node =
  | BindingRef of binding ref
  | ExprRef of expr ref
  | StmtRef of stmt ref
and uplink = reference_node ptr
and 'a node = { uplink: uplink; node: 'a }
and code = stmt list
and stmt = statement_node node
and statement_node =
  | If of expr * code * code
  | Binding of binding
  | Return of expr
and binding = binding_node node
and binding_node =
  | Let of binder * expr
  | LetFun of {
      binder: binder;
      parameters: binder list;
      body: expr;
    }
and binder = {
    kind : [ `Primitive of string | `Binder of int];
    mutable applied_occurrence: expr option;
    mutable binding_occurrence: binding ref option;
  }
and var = expr option ref DoublyList.t
and expr = expr_node node
and expr_node =
  | Apply of expr * expr list
  | Variable of var * binder Pointer.t

let get_binder = function
  | Let (binder, _) -> binder
  | LetFun { binder; _ } -> binder

let var binder =
  match binder.applied_occurrence with
  | None ->
     let self = ref None in
     let dlist = DoublyList.singleton self in
     let expr = { uplink = Pointer.null (); node = Variable (dlist, Pointer.ref binder) } in
     binder.applied_occurrence <- Some expr;
     self := Some expr;
     expr
  | Some { node = Variable (dlist, _); _ } ->
     let self = ref None in
     let dlist' = DoublyList.cons self dlist in
     let expr = { uplink = Pointer.null (); node = Variable (dlist', Pointer.ref binder) } in
     self := Some expr;
     expr
  | Some _ -> assert false

let fresh_binder () =
  { kind = `Binder 1; applied_occurrence = None; binding_occurrence = None }

let set_uplink reference { uplink; _ } =
  Pointer.set uplink reference

let let_fun binder parameters body =
  let node = LetFun { binder; parameters; body } in
  let binding = ref { uplink = Pointer.null (); node } in
  let reference = BindingRef binding in
  binder.binding_occurrence <- Some binding;
  List.iter (fun binder -> binder.binding_occurrence <- Some binding) parameters;
  set_uplink reference body;
  !binding

let make_identity () =
  let binder = fresh_binder () in
  let xb = binder in
  let x = var xb in
  let id = ref (let_fun (fresh_binder ()) [xb] x) in
  set_uplink (BindingRef id) x;
  !id

(* type label = string
 * type fun_kind =
 *   [ `Regular
 *   | `Star
 *   | `Async ]
 * and yield_kind =
 *   [ `Regular
 *   | `Star ]
 * type let_kind =
 *   [ `Const
 *   | `Let
 *   | `Var ]
 * type unary_op =
 *   [ `Plus
 *   | `Minus
 *   | `Increment of fixity
 *   | `Decrement of fixity
 *   | `Not
 *   | `BitwiseNot
 *   | `Typeof
 *   | `Delete
 *   | `Void ]
 * and fixity =
 *   [ `Prefix
 *   | `Postfix ]
 * type binary_op =
 *   [ `Plus
 *   | `Minus
 *   | `Times
 *   | `Division
 *   | `Mod
 *   | `Pow
 *   | `WeakEq
 *   | `StrictEq
 *   | `WeakNeq
 *   | `StrictNeq
 *   | `Lt
 *   | `Gt
 *   | `Ge
 *   | `Le
 *   | `BitwiseAnd
 *   | `BitwiseOr
 *   | `BitwiseXor
 *   | `LeftShift
 *   | `RightShift
 *   | `ZerofillRightShift
 *   | `Comma ]
 * 
 * type literal =
 *   | Bool of bool
 *   | Int of int
 *   | Float of float
 *   | Char of char
 *   | String of string
 * type expr =
 *   | Lit of literal
 *   | Var of Ident.var
 *   | Fun of fun_def
 *   | Arrow of Ident.binder list * js
 *   | Apply of expr * expr list
 *   | Unary of unary_op * expr
 *   | Binary of binary_op * expr * expr
 *   | Array of expr array
 *   | Obj of (label * expr) list
 *   | Project of [`Dot | `Subscript] * expr * string
 *   | Yield of { kind: yield_kind; expr: expr }
 *   | Await of expr
 * and stmt =
 *   | Expr of expr
 *   | Return of expr
 *   | If of expr * js * js option
 *   | Switch of expr * (label * js) list * js option
 *   | Try of js * catch_def
 *   | Assign of Ident.var * expr
 *   (\* | Skip *\)
 *   | Break
 *   | Continue
 *   | Binding of decl
 * and decl =
 *   | Let of { kind: let_kind; binder: Ident.binder; expr: expr }
 *   | LetFun of fun_def
 * and fun_def = {
 *     kind: fun_kind;
 *     fun_binder: [`Anonymous | `Binder of Ident.binder];
 *     params: Ident.binder list;
 *     fun_body: js
 *   }
 * and catch_def = {
 *     exn_binder: Ident.binder;
 *     catch_body: js
 *   }
 * (\* and decls = decl list *\)
 * and js = stmt list
 * and program = js
 * and reference_node =
 *   | BindingRef of binding ref
 *   | ExprRef of expr ref
 *   | StmtRef of stmt ref
 * and binder = {
 *     bkind : [`Primitive of string | `Binder of int];
 *     mutable applied_occurrence: expr option;
 *     mutable binding_occurrence: binding ref option;
 * } *)
