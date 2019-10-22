type t = int
         [@@deriving show]

let of_name : string -> t
  = fun name -> Hashtbl.hash name

let equal : t -> t -> bool
  = (=)

let compare : t -> t -> int
  = compare
