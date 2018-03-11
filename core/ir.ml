(** Monadic IR *)

open CommonTypes

[@@@ocaml.warning "-39"] (** disables warnings about unused rec flags **)

type scope = Var.scope
  [@@deriving show]
(* term variables *)
type var = Var.var
  [@@deriving show,eq,yojson]
type var_info = Var.var_info
  [@@deriving show]
type binder = Var.binder
  [@@deriving show]

(* type variables *)
type tyvar = Types.quantifier
  [@@deriving show]
type tyarg = Types.type_arg
  [@@deriving show]

type name = string
  [@@deriving show]

type name_set = Utility.stringset
  [@@deriving show]
type 'a name_map = 'a Utility.stringmap
  [@@deriving show]

type 'a var_map = 'a Utility.intmap
  [@@deriving show]

type language = string
  [@@deriving show]

type location = CommonTypes.Location.t
  [@@deriving show]

type value =
  | Constant   of Constant.t
  | Variable   of var
  | Extend     of value name_map * value option
  | Project    of name * value
  | Erase      of name_set * value
  | Inject     of name * value * Types.datatype

  | TAbs       of tyvar list * value
  | TApp       of value * tyarg list

  | XmlNode    of name * value name_map * value list
  | ApplyPure  of value * value list

  | Closure    of var * tyarg list * value

  | Coerce     of value * Types.datatype
and tail_computation =
  | Return     of value
  | Apply      of value * value list

  | Special    of special

  | Case       of value * (binder * computation) name_map * (binder * computation) option
  | If         of value * computation * computation
and fun_def = binder * (tyvar list * binder list * computation) * binder option * location
and binding =
  | Let        of binder * (tyvar list * tail_computation)
  | Fun        of fun_def
  | Rec        of fun_def list
  | Alien      of binder * name * language
  | Module     of string * binding list option
and special =
  | Wrong      of Types.datatype
  | Database   of value
  | Lens       of value * Types.lens_sort
  | LensDrop   of value * string * string * value * Types.lens_sort
  | LensSelect of value * Types.lens_phrase * Types.lens_sort
  | LensJoin   of value * value * string list * Types.lens_phrase * Types.lens_phrase * Types.lens_sort
  | LensGet    of value * Types.datatype
  | LensPut    of value * value * Types.datatype
  | Table      of value * value * value * (Types.datatype * Types.datatype * Types.datatype)
  | Query      of (value * value) option * computation * Types.datatype
  | Update     of (binder * value) * computation option * computation
  | Delete     of (binder * value) * computation option
  | CallCC     of value
  | Select     of name * value
  | Choice     of value * (binder * computation) name_map
  | Handle     of handler
  | DoOperation of name * value list * Types.datatype
and computation = binding list * tail_computation
and effect_case = binder * binder * computation
and handler = {
    ih_comp: computation;
    ih_cases: effect_case name_map;
    ih_return: binder * computation;
    ih_depth: handler_depth;
}
and handler_depth = | Deep of (binder * value) list | Shallow
  [@@deriving show]

let binding_scope : binding -> scope =
  function
  | Let (b, _)
  | Fun (b, _, _, _)
  | Rec ((b, _, _, _)::_)
  | Alien (b, _, _) -> Var.scope_of_binder b
  | Rec []
  | Module _ -> assert false

let binder_of_fun_def (fb, _, _, _) = fb

let tapp (v, tyargs) =
  match tyargs with
    | [] -> v
    | _ -> TApp (v, tyargs)

let letm (b, tc) = Let (b, ([], tc))
let letmv (b, v) = letm (b, Return v)

let rec is_atom =
  function
    | Constant (Constant.Bool  _)
    | Constant (Constant.Int   _)
    | Constant (Constant.Char  _)
    | Constant (Constant.Float _)
    | Variable _ -> true
(*
  This can only be an atom if
  Erase is just an upcast, and our language
  is properly parametric.
*)
(*    | Erase (_, v) *)
    | Coerce (v, _) -> is_atom v
    | _ -> false

let with_bindings bs' (bs, tc) = (bs' @ bs, tc)

type program = computation
  [@@deriving show]

let string_of_var = string_of_int

let string_of_value = show_value
let string_of_tail_computation = show_tail_computation
let string_of_binding = show_binding
let string_of_special  = show_special
let string_of_computation = show_computation
let string_of_program = show_program

module ProcedureFragmentation =
  struct

    type liveset = IntSet.t
    type liveness_map = liveset IntMap.t
    module LivenessMonad = struct
      type st = liveset * liveness_map
      type 'a t = st -> ('a * st)

      let pure : 'a -> 'a t
        = fun x st -> (x, st)

      let run ~init m =
        m init

      let kill : Var.var -> unit t
        = fun v (ls, lm) ->
        ((), (IntSet.remove v ls, lm))

      let use : Var.var -> unit t
        = fun v (ls, lm) ->
        ((), (IntSet.add v ls, lm))

      let register : Var.var -> unit t
        = fun v (ls, lm) ->
        ((), (ls, IntMap.add v ls lm))

      let get_liveset : liveset t
        = fun (ls, lm) -> (ls, (ls, lm))

      let put_liveset ls ->
          fun (_, lm) -> ((), (ls, lm))

      let union : liveset list -> unit t
        = fun lss (ls, lm) ->
        let ls =
          List.fold_left
            (fun ls ls' ->
              IntSet.union ls ls')
            ls lss
        in
        ((), (ls, lm))

      let (>>=) m k =
        fun st ->
        let (x, st') = run ~init:st m in
        run ~init:st' (k x)
    end
    (* Fine-grained liveness analysis.
       Computes a mapping from binders to the live set at their
       definition point. *)
    let liveness tyenv =
      let open LivenessMonad in
      let rec value : value -> unit LivenessMonad.t
        = fun v ->
        match v with
        | `Variable v -> use v
        | `Extend (fields, r) ->
           let m =
             StringMap.fold
               (fun _ v m ->
                 m >>= (fun () -> value v))
               fields (pure ())
           in
           begin match r with
           | None -> m
           | Some v ->
              m >>= (fun () -> value v)
           end
        | `Project (_, v)
        | `Erase (_, v)
        | `Inject (_, v, _)
        | `TAbs (_, v)
        | `TApp (v, _)
        | `Closure (_, v)
        | `Coerce (v, _) -> value v
        | `ApplyPure (v, vs) ->
           let m = value v in
           List.fold_left
             (fun m v ->
               m >>= (fun () -> value v))
             m vs
      in
      assert false


    (* Precondition: closure conversion *)
    let fragmentise tyenv =
      object (o)
        inherit Transform.visitor(tyenv)

        val basename = ""
        method set_basename name =
          {< basename = name >}

        val n = ref (-1)
        method fresh_frame_name =
          incr n; Printf.sprintf "%s%d" basename !n

        val frames : fun_def list = []
        method add_frame f =
          {< frames = f :: frames >}
        method get_frames = frames

        val live_vars : Var.binder list = []
        method add_live_var b =
          {< live_vars = live_vars @ [b] >} (* TODO FIXME: choose a clever order preserving data structure *)
        method add_live_vars bs =
          {< live_vars = live_vars @ bs >}

        method! computation (bs, tc) =
          assert false

        method! binding = function
          | _ -> assert false

        method fun_def = function
          | `Fun (binder, (_tyvars, _formal_params, body), _clo, _location) ->
             (* let o = o#add_live_vars formal_params in (\* TODO FIXME: too conservative liveness estimate *\) *)
             let fname = Var.name_of_binder binder in
             let o = o#set_basename fname in
             o#computation body
          | `Rec _fd -> assert false
      end

  let procedure : Types.datatype Env.Int.t -> [`Fun of fun_def | `Rec of fun_def list] -> [ `Rec of fun_def list ] list
    = fun env ->
    function
    | `Fun _fd -> assert false
    | `Rec _fd -> assert false
end
