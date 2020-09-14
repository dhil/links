module type S = sig
  type s
  type t

  val make : (unit -> int) -> t
  val from : s -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val to_string : t -> string

  module Map : Utility.Map with type key = t
  module Set : Utility.Set with type elt = t
end

module Legacy : S = struct
  type s = string
  type t = s

  let make
    = fun gensym -> Printf.sprintf "_%d" (gensym ())

  let from s = s

  let compare = String.compare
  let equal = String.equal

  let to_string s = s

  module Map = Utility.StringMap
  module Set = Utility.StringSet
end
