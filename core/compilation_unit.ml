open Utility

type state = Empty
           | Loaded
           | Ready

type t =
  { id: int;
    mutable source: Sugartypes.program * Scanner.position_context;
    mutable aliens: string list;
    mutable linkset: int list;
    mutable state: state }

let dummy_source = ([], None), new SourceCode.source_code

let id = ref (-1)

let empty : unit -> t
  = fun () ->
  { id = (incr id; !id);
    source = dummy_source;
    aliens = [];
    linkset = [];
    state = Empty }

let make_ready : unit -> t
  = fun () ->
  let comp_unit = empty () in
  comp_unit.state <- Ready; comp_unit

let is_synthetic : t -> bool
  = fun { source; _ } -> source == dummy_source (* Pointer equality. *)

let is_loaded : t -> bool = function
  | { state = Empty; _ } -> false
  | _ -> true

let promote : t -> unit
  = fun comp_unit ->
  match comp_unit.state with
  | Empty -> comp_unit.state <- Loaded
  | Loaded -> comp_unit.state <- Ready
  | _ -> assert false (* TODO FIXME. *)

let is_ready : t -> bool = function
  | { state = Ready; _ } -> true
  | _ -> false

let to_string : t -> string
  = fun { id; aliens; linkset; state; _ } ->
  let aliens =
    Printf.sprintf "[%s]" (String.concat ";" aliens)
  in
  let linkset =
    Printf.sprintf "[%s]" (String.concat ";" (List.map string_of_int linkset))
  in
  Printf.sprintf "{ id = %d; aliens = %s; linkset = %s; state = %s; }" id aliens linkset (match state with Empty -> "Empty" | Loaded -> "Loaded" | Ready -> "Ready")

let dummy = empty ()
