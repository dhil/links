type t = int ref
let make ?(start=0) () =
  assert (start >= 0);
  ref start

let next g =
  incr g; !g
