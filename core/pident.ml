open Utility

type t = int
         [@@deriving show]

let of_name : string -> t
  = fun name -> Hashtbl.hash name

let equal : t -> t -> bool
  = (=)

let compare : t -> t -> int
  = compare

(* Necessary to break the cyclic definition `type t = t` since
   ppx_deriving doesn't support `type nonrec t = t`. *)
type s = t [@@deriving show]
module Map = Map.Make(struct type t = s [@@deriving show] let compare = compare end)
module Set = Set.Make(struct type t = s [@@deriving show] let compare = compare end)
