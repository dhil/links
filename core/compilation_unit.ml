open Utility

module LinkSet = struct
  type 'a t = 'a StringTrie.t

  let empty : unit -> 'a t
    = fun () -> StringTrie.empty

  let union : 'a t -> 'a t -> 'a t =
    fun ls ls' -> StringTrie.union (fun _ _ v -> Some v) ls ls'

  let add : string list -> 'a -> 'a t -> 'a t
    = fun prefix pos ls -> StringTrie.add prefix pos ls
end

type state = Empty
           | Loaded

type colour = White
            | Black
            | Grey

type t =
  { mutable source: Sugartypes.program * Scanner.position_context;
    mutable aliens: string list;
    mutable linkset: t LinkSet.t;
    mutable state: state;
    mutable colour: colour }

let dummy_source = ([], None), new SourceCode.source_code

let empty : unit -> t
  = fun () ->
  { source = dummy_source;
    aliens = [];
    linkset = StringTrie.empty;
    state = Empty;
    colour = White }

let to_colour : t -> colour = function
  | { colour; _ } -> colour

let paint : t -> colour -> unit
  = fun comp_unit colour -> comp_unit.colour <- colour

let string_of_colour = function
  | White -> "White"
  | Black -> "Black"
  | Grey -> "Grey"

let is_loaded : t -> bool = function
  | { state = Loaded; _ } -> true
  | _ -> false

let promote : t -> t
  = fun comp_unit ->
  comp_unit.state <- Loaded; comp_unit

let to_string : t -> string
  = fun { aliens; linkset; state; colour; _ } ->
  let aliens =
    Printf.sprintf "[%s]" (String.concat ";" aliens)
  in
  let linkset =
    Printf.sprintf "[%s]" (String.concat ";" (StringTrie.fold (fun prefix _ acc -> String.concat "." prefix :: acc) linkset []))
  in
  Printf.sprintf "{ aliens = %s; linkset = %s; state = %s; colour = %s }" aliens linkset (match state with Empty -> "Empty" | Loaded -> "Loaded") (string_of_colour colour)
