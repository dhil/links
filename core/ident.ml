module type IDENTIFIABLE = sig
  type t [@@deriving show]
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

type t = int [@@deriving show]
let make x = x
let fresh g = Gensym.next g
let to_string = string_of_int
let equal : t -> t -> bool = (=)
let compare : t -> t -> int = Stdlib.compare

(* Necessary to break the cyclic definition `type t = t` since
   ppx_deriving doesn't support `type nonrec t = t`. *)
type s = t [@@deriving show]
module Map = Utility.Map.Make(struct type t = s [@@deriving show] let compare = compare end)
module Set = Utility.Set.Make(struct type t = s [@@deriving show] let compare = compare end)

(* Compilation unit names, interface names. *)
module Persistent = struct
  type t = int [@@deriving show]

  let of_string : string -> t
    = fun name -> Hashtbl.hash name

  let to_string : t -> string
    = fun _ -> assert false

  let equal : t -> t -> bool
    = (=)

  let compare : t -> t -> int
    = compare

  type s = t [@@deriving show]
  module Map = Utility.Map.Make(struct type t = s [@@deriving show] let compare = compare end)
  module Set = Utility.Set.Make(struct type t = s [@@deriving show] let compare = compare end)
end

module type VAR = sig
  type root
  include IDENTIFIABLE
  val make : root -> Persistent.t list -> t
  val root : t -> root
  val path : t -> Persistent.t list
end

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

  let root : t -> I.t
    = fun { root; _ } -> root

  let path : t -> Persistent.t list
    = fun { path; _ } -> path
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
