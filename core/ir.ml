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
module type ITER =
sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method lookup_type : var -> Types.datatype
    method constant : constant -> 'self_type
    method option :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a option -> 'self_type
    method list :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a list -> 'self_type
    method name_map :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a name_map -> 'self_type
    method var_map :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a var_map -> 'self_type
    method var : var -> 'self_type
    method value : value -> 'self_type

    method tail_computation : tail_computation -> 'self_type
    method special : special -> 'self_type
    method bindings : binding list -> 'self_type
    method computation : computation -> 'self_type
    method binding : binding -> 'self_type
    method binder : binder -> 'self_type

    method program : program -> 'self_type

    method get_type_environment : environment
  end
end

module Iter : ITER =
struct
  open Types

  type environment = datatype Env.Int.t

  let info_type (t, _, _) = t

  module Env = Env.Int

  class visitor (tyenv : environment) =
  object ((o : 'self_type))
    val tyenv = tyenv
    (* val cenv = Env.empty *)

    method lookup_type : var -> datatype = fun var ->
      Env.lookup tyenv var

    method constant : constant -> 'self_type
      = fun _ -> o

    method option :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a option -> 'self_type =
      fun f v ->
        match v with
        | None -> o
        | Some v -> f o v

    method list :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a list -> 'self_type =
      fun f v ->
        List.fold_left f o v

    method name_map :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a name_map -> 'self_type =
      fun f vmap ->
        StringMap.fold
          (fun _ v o -> f o v)
          vmap o

    method var_map :
      'a.
      ('self_type -> 'a -> 'self_type) ->
      'a var_map -> 'self_type =
      fun f vmap ->
        IntMap.fold
          (fun _name v o -> f o v)
          vmap o

    method var : var -> 'self_type =
      fun _ -> o

    method value : value -> 'self_type =
      function
        | `Constant c -> o#constant c
        | `Variable x -> o#var x
        | `Extend (fields, base) ->
            let o = o#name_map (fun o -> o#value) fields in
            o#option (fun o -> o#value) base
        | `Project (_, v) ->
            o#value v
        | `Erase (_, v) ->
            o#value v
        | `Inject (_, v, _) ->
            o#value v
        | `TAbs (_tyvars, v) ->
            o#value v
        | `TApp (v, _ts) ->
            o#value v
        | `XmlNode (_tag, attributes, children) ->
            let o = o#name_map (fun o -> o#value) attributes in
            o#list (fun o -> o#value) children
        | `ApplyPure (f, args) ->
            let o = o#value f in
            o#list (fun o -> o#value) args
        | `Closure (f, z) ->
            let o = o#var f in
            o#value z
        | `Coerce (v, _t) ->
           o#value v

    method tail_computation : tail_computation -> 'self_type =
      function
        | `Return v -> o#value v
        | `Apply (f, args) ->
           let o = o#value f in
           o#list (fun o -> o#value) args
        | `Special special -> o#special special
        | `Case (v, cases, default) ->
            let o = o#value v in
            let o =
              o#name_map
                (fun o (b, c) ->
                   let o = o#binder b in
                   o#computation c)
                cases
            in
            o#option
              (fun o (b, c) ->
                let o = o#binder b in
                o#computation c)
              default
        | `If (v, left, right) ->
            let o = o#value v in
            let o = o#computation left in
            o#computation right

    method special : special -> 'self_type =
      function
        | `Wrong _t -> o
        | `Database v -> o#value v
        | `Table (db, table_name, keys, _tt) ->
            let o = o#value db in
            let o = o#value keys in
            o#value table_name
        | `Query (range, e, _) ->
            let o =
              o#option
                (fun o (limit, offset) ->
                   let o = o#value limit in
                   o#value offset)
                range
            in
            o#computation e
        | `Update ((x, source), where, body) ->
            let o = o#value source in
            let o = o#binder x in
            let o = o#option (fun o -> o#computation) where in
            o#computation body
        | `Delete ((x, source), where) ->
            let o = o#value source in
            let o = o#binder x in
            o#option (fun o -> o#computation) where
        | `CallCC v -> o#value v
        | `Select (_l, v) -> o#value v
        | `Choice (v, bs) ->
           let o = o#value v in
           o#name_map
             (fun o (b, c) ->
               let o = o#binder b in
               o#computation c)
             bs
	| `Handle ({ ih_comp; ih_cases; ih_return; ih_depth }) ->
	   let o = o#computation ih_comp in
           let o =
             match ih_depth with
             | `Deep params ->
                List.fold_left
                    (fun o (b,v) ->
                      let o = o#binder b in
                      o#value v)
                  o params
             | _ -> o
           in
	   let o =
	     o#name_map
               (fun o (x, resume, c) ->
                 let o = o#binder x in
		 let o = o#binder resume in
		 o#computation c)
	       ih_cases
	   in
           let o = o#binder (fst ih_return) in
           o#computation (snd ih_return)
	| `DoOperation (_name, vs, _t) ->
           o#list (fun o -> o#value) vs

   method bindings : binding list -> 'self_type =
      fun bs ->
        List.fold_left
          (fun o b -> o#binding b)
          o bs

    method computation : computation -> 'self_type =
      fun (bs, tc) ->
        let o = o#bindings bs in
        o#tail_computation tc

    method binding : binding -> 'self_type =
      function
        | `Let (x, (_tyvars, tc)) ->
            let o = o#binder x in
            o#tail_computation tc
        | `Fun (f, (_tyvars, xs, body), z, _location) ->
           let o = o#option (fun o -> o#binder) z in
           let o =
             List.fold_right
               (fun x o -> o#binder x)
               xs o
           in
           let o = o#computation body in
           o#binder f
        | `Rec defs ->
            let o =
              List.fold_right
                (fun (f, _, _, _) o -> o#binder f)
                defs o
            in
            List.fold_left
              (fun (o : 'self_type) (_, (_, xs, body), z, _) ->
                let o = o#option (fun o -> o#binder) z in
                let o =
                  List.fold_right
                    (fun x o -> o#binder x)
                    xs o
                in
                o#computation body)
              o defs
        | `Alien (x, _name, _language) -> o#binder x
        | `Module (_name, defs) ->
           o#option (fun o -> o#bindings) defs

    method binder : binder -> 'self_type =
      fun (var, info) ->
        let tyenv = Env.bind tyenv (var, info_type info) in
        {< tyenv=tyenv >}

    method program : program -> 'self_type = o#computation

    method get_type_environment : environment = tyenv
  end
end

module ProcedureFragmentation =
  struct

  (*   type liveset = IntSet.t *)
  (*   type liveness_map = liveset IntMap.t *)
  (*   module LivenessMonad = struct *)
  (*     type st = liveset * liveness_map *)
  (*     type 'a t = st -> ('a * st) *)

  (*     let pure : 'a -> 'a t *)
  (*       = fun x st -> (x, st) *)

  (*     let run ~init m = *)
  (*       m init *)

  (*     let kill : Var.var -> unit t *)
  (*       = fun v (ls, lm) -> *)
  (*       ((), (IntSet.remove v ls, lm)) *)

  (*     let use : Var.var -> unit t *)
  (*       = fun v (ls, lm) -> *)
  (*       ((), (IntSet.add v ls, lm)) *)

  (*     let register : Var.var -> unit t *)
  (*       = fun v (ls, lm) -> *)
  (*       ((), (ls, IntMap.add v ls lm)) *)

  (*     let get_liveset : liveset t *)
  (*       = fun (ls, lm) -> (ls, (ls, lm)) *)

  (*     let put_liveset ls = *)
  (*         fun (_, lm) -> ((), (ls, lm)) *)

  (*     let union : liveset list -> unit t *)
  (*       = fun lss (ls, lm) -> *)
  (*       let ls = *)
  (*         List.fold_left *)
  (*           (fun ls ls' -> *)
  (*             IntSet.union ls ls') *)
  (*           ls lss *)
  (*       in *)
  (*       ((), (ls, lm)) *)

  (*     let (>>=) m k = *)
  (*       fun st -> *)
  (*       let (x, st') = run ~init:st m in *)
  (*       run ~init:st' (k x) *)
  (*   end *)
  (*   (\* Fine-grained liveness analysis. *)
  (*      Computes a mapping from binders to the live set at their *)
  (*      definition point. *\) *)
  (*   let liveness tyenv prog = *)
  (*     let open LivenessMonad in *)
  (*     let rec value : value -> unit LivenessMonad.t *)
  (*       = function *)
  (*       | `Variable v -> use v *)
  (*       | `Extend (fields, r) -> *)
  (*          let m = *)
  (*            StringMap.fold *)
  (*              (fun _ v m -> *)
  (*                m >>= (fun () -> value v)) *)
  (*              fields (pure ()) *)
  (*          in *)
  (*          begin match r with *)
  (*          | None -> m *)
  (*          | Some v -> *)
  (*             m >>= (fun () -> value v) *)
  (*          end *)
  (*       | `Project (_, v) *)
  (*       | `Erase (_, v) *)
  (*       | `Inject (_, v, _) *)
  (*       | `TAbs (_, v) *)
  (*       | `TApp (v, _) *)
  (*       | `Closure (_, v) *)
  (*       | `Coerce (v, _) -> value v *)
  (*       | `ApplyPure (v, vs) -> *)
  (*          let m = value v in *)
  (*          List.fold_left *)
  (*            (fun m v -> *)
  (*              m >>= (fun () -> value v)) *)
  (*            m vs *)
  (*       | _ -> pure () *)
  (*     and tail_computation : tail_computation -> unit LivenessMonad.t *)
  (*     = function *)
  (*     | `Return v -> value v *)
  (*     | `Apply (f, vs) -> *)
  (*        let m = value f in *)
  (*        List.fold_left *)
  (*          (fun m v -> *)
  (*            m >>= (fun () -> value v)) *)
  (*          m vs *)
  (*     | `Special s -> special s *)
  (*     | `Case (scrutinee, cases, default) -> *)
  (*        let cases = *)
  (*          let cases = *)
  (*            List.map (fun (_,c) -> c) (StringMap.to_alist cases) *)
  (*          in *)
  (*          match default with *)
  (*          | Some case -> case :: cases *)
  (*          | None -> cases *)
  (*        in *)
  (*        get_liveset >>= fun ls -> *)
  (*        let m = *)
  (*          List.fold_left *)
  (*            (fun m (b, comp) -> *)
  (*              let v = Var.var_of_binder b in *)
  (*              get_liveset >>= fun ls -> *)
  (*              computation comp >>= fun () -> *)
  (*              kill v >>= fun () -> *)
  (*              register v >>= fun () -> *)
  (*              get_liveset >>= fun ls' -> *)
  (*              put_liveset ls >>= fun () -> *)
  (*              m >>= fun lss -> *)
  (*              pure (ls' :: lss)) *)
  (*            (pure []) cases *)
  (*        in *)
  (*        m >>= union >>= fun () -> *)
  (*        value scrutinee *)
  (*     | `If (cond, tt, ff) -> *)
  (*        get_liveset >>= fun ls -> *)
  (*        computation tt >>= fun () -> *)
  (*        get_liveset >>= fun ls' -> *)
  (*        put_liveset ls >>= fun () -> *)
  (*        computation ff >>= fun () -> *)
  (*        get_liveset >>= fun ls'' -> *)
  (*        union [ls'; ls''] >>= fun () -> *)
  (*        value cond *)
  (*     and special : special -> unit LivenessMonad.t *)
  (*     = function *)
  (*     | `Wrong _ -> pure () *)
  (*     | `DoOperation (_, vs, _) -> *)
  (*        let m = pure () in *)
  (*        List.fold_left *)
  (*          (fun m v -> *)
  (*            m >>= (fun () -> value v)) *)
  (*          m vs *)
  (*     | _ -> failwith "Not yet implemented" *)
  (*     and computation : computation -> unit LivenessMonad.t *)
  (*     = fun (bs, tc) -> *)
  (*       List.fold_left *)
  (*         (fun m b -> *)
  (*           m >>= fun() -> *)
  (*           binding b) *)
  (*         (tail_computation tc) bs *)
  (*     and binding : binding -> unit LivenessMonad.t *)
  (*     = function *)
  (*     | `Let (b, (_, tc)) -> *)
  (*       let v = Var.var_of_binder b in *)
  (*       tail_computation tc >>= fun () -> *)
  (*       kill v >>= fun () -> *)
  (*       register v *)
  (*     | `Fun (b, (_, bs, comp), b', _) -> *)
  (*        let bs = *)
  (*          match b' with *)
  (*          | Some b' -> b' :: b :: bs *)
  (*          | None -> b :: bs *)
  (*        in *)
  (*        let m = *)
  (*          List.fold_left *)
  (*            (fun m b -> *)
  (*              let v = Var.var_of_binder b in *)
  (*              m >>= fun () -> *)
  (*              kill v) *)
  (*            (computation comp) bs *)
  (*        in *)
  (*        List.fold_left *)
  (*          (fun m b -> *)
  (*            let v = Var.var_of_binder b in *)
  (*            m >>= fun () -> *)
  (*            register v) *)
  (*          m bs *)
  (*     | `Rec fundefs -> *)
  (*        let rec_def (_, (_, bs, comp), b', _) = *)
  (*          let bs = *)
  (*            match b' with *)
  (*            | Some b' -> b' :: bs *)
  (*            | None -> bs *)
  (*          in *)
  (*          let m = *)
  (*            List.fold_left *)
  (*              (fun m b -> *)
  (*                let v = Var.var_of_binder b in *)
  (*                m >>= fun () -> *)
  (*                kill v) *)
  (*              (computation comp) bs *)
  (*          in *)
  (*          List.fold_left *)
  (*            (fun m b -> *)
  (*              let v = Var.var_of_binder b in *)
  (*              m >>= fun () -> *)
  (*              register v) *)
  (*            m bs *)
  (*        in *)
  (*        List.fold_left *)
  (*          (fun m f -> *)
  (*            get_liveset >>= fun ls -> *)
  (*            rec_def f >>= fun () -> *)
  (*            put_liveset ls) *)
  (*          (pure ()) fundefs *)
  (*     | `Alien (b, _, _) -> *)
  (*        let v = Var.var_of_binder b in *)
  (*        kill v >>= fun () -> *)
  (*        register v *)
  (*     | _ -> assert false *)
  (*     in *)
  (*     let (_, (_, lm)) = *)
  (*       LivenessMonad.run ~init:(IntSet.empty, IntMap.empty) (computation prog) *)
  (*     in lm *)


  (*   (\* Precondition: closure conversion *\) *)
  (*   let fragmentise tyenv = *)
  (*     object (o) *)
  (*       inherit Transform.visitor(tyenv) *)

  (*       val basename = "" *)
  (*       method set_basename name = *)
  (*         {< basename = name >} *)

  (*       val n = ref (-1) *)
  (*       method fresh_frame_name = *)
  (*         incr n; Printf.sprintf "%s%d" basename !n *)

  (*       val frames : fun_def list = [] *)
  (*       method add_frame f = *)
  (*         {< frames = f :: frames >} *)
  (*       method get_frames = frames *)

  (*       val live_vars : Var.binder list = [] *)
  (*       method add_live_var b = *)
  (*         {< live_vars = live_vars @ [b] >} (\* TODO FIXME: choose a clever order preserving data structure *\) *)
  (*       method add_live_vars bs = *)
  (*         {< live_vars = live_vars @ bs >} *)

  (*       method! computation (bs, tc) = *)
  (*         assert false *)

  (*       method! binding = function *)
  (*         | _ -> assert false *)

  (*       method fun_def = function *)
  (*         | `Fun (binder, (_tyvars, _formal_params, body), _clo, _location) -> *)
  (*            (\* let o = o#add_live_vars formal_params in (\\* TODO FIXME: too conservative liveness estimate *\\) *\) *)
  (*            let fname = Var.name_of_binder binder in *)
  (*            let o = o#set_basename fname in *)
  (*            o#computation body *)
  (*         | `Rec _fd -> assert false *)
  (*     end *)

  (* let procedure : Types.datatype Env.Int.t -> [`Fun of fun_def | `Rec of fun_def list] -> [ `Rec of fun_def list ] list *)
  (*   = fun env -> *)
  (*   function *)
  (*   | `Fun _fd -> assert false *)
    (*   | `Rec _fd -> assert false *)
    let liveness _ _ = assert false
    let procedure _ _prog = assert false
  end

module TreeShaking =
struct

  type name_env = string IntMap.t
    deriving (Show)

  let name_env tyenv =
    object (o)
      inherit Iter.visitor(tyenv)

      val nenv = IntMap.empty
      method add var name =
        {< nenv = IntMap.add var name nenv >}
      method get_nenv = nenv

      method! binder (var, (_, name, _)) =
        o#add var name
    end

  type usage_map = IntSet.t IntMap.t
    deriving (Show)

  (* Computes a map from function variables and global let bound
     variables to a set of variables used in their bodies *)
  let usage_map tyenv =
    object (o)
      inherit Iter.visitor(tyenv) as super

      val scope_owner = (-1)
      method with_scope_owner owner =
        {< scope_owner = owner >}

      val usage_map : IntSet.t IntMap.t = IntMap.(add (-1) IntSet.empty empty)
      method get_usage_map = usage_map

      method use (user : Var.var) (usee : Var.var) =
        (* Printf.printf "%d uses %d\n%!" user usee; *)
        let usees =
          try
            IntMap.find user usage_map
          with
          | NotFound _ -> IntSet.empty
        in
        let usees = IntSet.add usee usees in
        {< usage_map = IntMap.add user usees usage_map >}

      method init v =
        {< usage_map = IntMap.add v IntSet.empty usage_map >}

      method! var var =
        o#use scope_owner var

      method! binding = function
      | `Let (b, (_, tc)) ->
         let (_, _, loc) = Var.info_of_binder b in
         let v = Var.var_of_binder b in
         if loc = `Global then
           let so = scope_owner in
           let o = o#init v in
           let o = o#with_scope_owner v in
           let o = o#tail_computation tc in
           o#with_scope_owner so
         else
           o#tail_computation tc
      | `Fun (b, (_, _, comp), _, _) ->
         let v = Var.var_of_binder b in
         let so = scope_owner in
         let o = o#init v in
         let o = o#with_scope_owner v in
         let o = o#computation comp in
         o#with_scope_owner so
      | `Rec fundefs ->
         (* initialise all scopes *)
         let o =
           List.fold_left
             (fun o (f, _, _, _) ->
               let v = Var.var_of_binder f in
               o#init v)
             o fundefs
         in
        (* visit the scopes one by one *)
         List.fold_left
           (fun o (f, (_, _, comp), _, _) ->
             let v = Var.var_of_binder f in
             let so = scope_owner in
             let o = o#with_scope_owner v in
             let o = o#computation comp in
             o#with_scope_owner so)
           o fundefs
      | `Alien (b, _, _) ->
         let v = Var.var_of_binder b in
         o#init v
      | _ -> assert false

    end

  (* Computes the set of variable used directly by the top-level tail
     computation (i.e. the "main" computation) *)
  let code_used_directly_by_main tyenv =
    object (o)
      inherit Iter.visitor(tyenv)

      val uses = IntSet.empty
      method use var = {< uses = IntSet.add var uses >}
      method get_uses = uses

      method! program (_, tc) =
        o#tail_computation tc

      method! var var = o#use var
    end

  (* Eliminates function and global let definitions which are not used
     directly or indirectly by the main computation. *)
  let eliminator tyenv reachable =
    object (o)
      inherit Transform.visitor(tyenv) as super

      method is_reachable b =
        let v = Var.var_of_binder b in
        IntSet.mem v reachable

      method is_global b =
        let (_, _, loc) = Var.info_of_binder b in
        loc = `Global

      method! bindings bs =
        let bs =
          List.fold_left
            (fun bs ->
              function
              | `Fun (fb, _, _, _) when not (o#is_reachable fb) -> bs
              | `Rec fundefs ->
                 begin match fundefs with
                 | [] -> bs
                 | (fb, _, _, _) :: _ when not (o#is_reachable fb) -> bs
                 | _ -> (`Rec fundefs) :: bs
                 end
              | `Let (b, _) when o#is_global b && not (o#is_reachable b) -> bs
              | b -> b :: bs)
            [] bs
        in
        super#bindings (List.rev bs)
    end

  let program tyenv prog =
    let nenv =
      let o = (name_env tyenv)#program prog in
      o#get_nenv
    in
    let usage_map =
      let o = (usage_map tyenv)#program prog in
      o#get_usage_map
    in
    let main_uses =
      let o = (code_used_directly_by_main tyenv)#program prog in
      o#get_uses
    in
    (* Printf.printf "nenv: %s\n%!" (Show_name_env.show nenv); *)
    (* Printf.printf "Main_uses: %s\n%!" (IntSet.Show_t.show main_uses); *)
    (* Printf.printf "Usage map: %s\n%!" (Show_usage_map.show usage_map); *)
    (* All reachable definitions from main (i.e. live code) are
       computed as the least fix point *)
    let rec lfp main_uses usage_map =
      let live' =
        IntSet.fold
          (fun v live ->
            try
              IntSet.union (IntMap.find v usage_map) live
            with
              NotFound _ (* occurs when v is primitive *) -> IntSet.add v live)
          main_uses main_uses
      in
      if IntSet.cardinal live' > IntSet.cardinal main_uses
      then lfp live' usage_map
      else live'
    in
    let liveset = lfp main_uses usage_map in
    (* Printf.printf "Live: %s\n%!" (IntSet.Show_t.show liveset); *)
    let (prog, _, _) = (eliminator tyenv liveset)#program prog in
    prog
end
