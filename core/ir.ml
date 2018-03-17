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

(* Computes a map from vars to names; useful for debugging. *)
module NameMap =
struct
  type name_map = string IntMap.t
    deriving (Show)

  let compute tyenv prog =
    ((object (o)
      inherit Iter.visitor(tyenv)

      val nenv = IntMap.empty
      method add var name =
        {< nenv = IntMap.add var name nenv >}
      method get_nenv = nenv

      method! binder (var, (_, name, _)) =
        o#add var name
     end)#program prog)#get_nenv
end

(* Tree shaking includes "live code", i.e. code that will be run
   directly or indirectly by the module's main
   computation. Though, we are somewhat conservative with regard
   to let bindings. *)
module TreeShaking =
struct

  type usage_map = IntSet.t IntMap.t
    deriving (Show)

  (* Computes a map from functions to a set of variables used in their
     bodies *)
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
      | `Fun (b, (_, _, comp), _, _) ->
         let o = o#binder b in
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
               let o = o#binder f in
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
         let o = o#binder b in
         let v = Var.var_of_binder b in
         o#init v
      | e -> super#binding e

    end

  (* Computes the set of variables used directly by the top-level tail
     computation (i.e. the "main" computation) *)
  let code_used_directly_by_main tyenv =
    object (o)
      inherit Iter.visitor(tyenv)

      val uses = IntSet.empty
      method use var = {< uses = IntSet.add var uses >}
      method get_uses = uses

      method! program (bs, tc) =
        let o =
          List.fold_left
            (fun o b ->
              match b with
              | `Let (b, (_, tc)) ->
                 let var = Var.var_of_binder b in
                 let o = o#use var in
                 o#tail_computation tc
              | _ -> o)
            o bs
        in
        o#tail_computation tc

      method! var var = o#use var
    end

  (* Eliminates function which are not used directly or indirectly by
     the main computation. *)
  let eliminator tyenv reachable =
    object (o)
      inherit Transform.visitor(tyenv) as super

      method is_reachable b =
        let v = Var.var_of_binder b in
        IntSet.mem v reachable

      (* method is_global b = *)
      (*   let (_, _, loc) = Var.info_of_binder b in *)
      (*   loc = `Global *)

      method! bindings bs =
        let bs =
          List.fold_left
            (fun bs ->
              function
              | `Fun (fb, _, _, _) when not (o#is_reachable fb) -> bs
              | `Rec fundefs ->
                 let fundefs' =
                   List.filter (fun (fb, _, _, _) -> o#is_reachable fb) fundefs
                 in
                 if fundefs' = []
                 then bs
                 else (`Rec fundefs') :: bs
              | b -> b :: bs)
            [] bs
        in
        super#bindings (List.rev bs)
    end

  let program tyenv prog =
    let usage_map =
      let o = (usage_map tyenv)#program prog in
      o#get_usage_map
    in
    let main_uses =
      let o = (code_used_directly_by_main tyenv)#program prog in
      o#get_uses
    in
    Printf.eprintf "Main_uses: %s\n%!" (IntSet.Show_t.show main_uses);
    Printf.eprintf "Usage map: %s\n%!" (Show_usage_map.show usage_map);
    (* All reachable definitions from main (i.e. live code) are
       computed as the least fix point *)
    let rec lfp main_uses usage_map =
      let live' =
        IntSet.fold
          (fun v live ->
            try
              (* Printf.eprintf "Intermediate live set: %s\n%!" (IntSet.Show_t.show live); *)
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
    (* let liveness_map = ProcedureFragmentation.liveness tyenv prog in *)
    (* Printf.printf "Liveness: %s\n%!" (ProcedureFragmentation.Show_liveness_map.show liveness_map); *)
    prog
end

(** In some cases the pattern matching compiler introduces trivial
    bindings of the form

    fun f(p_0,...,p_n) { var x = p_i; M }, such that p_i \notin FV(M), for some i \in {0,...,n}.

    This pass eliminates some of these bindings by hoisting their
    binders into the function's parameter list. *)
module TidyBindings : sig
  val program : Types.datatype Env.Int.t -> program -> program
end = struct

  let program tyenv prog =
    let eliminator tyenv =
      object (o)
        inherit Transform.visitor(tyenv) as super

        val sigma : binder IntMap.t = IntMap.empty
        val binders = IntSet.empty

        method get_sigma = sigma
        method add_parameter b =
          let var = Var.var_of_binder b in
          {< binders = IntSet.add var binders >}
        method backup = (sigma, binders)
        method restore (sigma, binders) =
          {< sigma = sigma; binders = binders; >}
        method reset =
          {< sigma = IntMap.empty; binders = IntSet.empty; >}
        method get_binders = binders
        method replace var b =
          if IntMap.mem var o#get_sigma && Var.var_of_binder (IntMap.find var o#get_sigma) <> Var.var_of_binder b
          then failwith (Printf.sprintf "Unsound binder hoisting: %d has already been replaced." var) (* Should *never* happen *)
          else {< sigma = IntMap.add var b o#get_sigma >}

        method is_function_parameter = function
        | `Variable v -> IntSet.mem v o#get_binders
        | _ -> false

        method get_variable = function
        | `Variable v -> v
        | _ -> assert false

        method! binding = function
        | `Fun (fb, (tyvars, xsb, body), z, loc) ->
           let envs = o#backup in
           let o = o#reset in
           let (xsb, o) =
             List.fold_left
               (fun (bs, o) b ->
                 let (b, o) = o#binder b in
                 (b :: bs, o#add_parameter b))
               ([], o) xsb
           in
           let (z, o) = o#optionu (fun o -> o#binder) z in
           let (body, _, o) = o#computation body in
           let (fb, o) = o#binder fb in
           let xsb =
             let replace sigma b =
               let var = Var.var_of_binder b in
               if IntMap.mem var sigma then IntMap.find var sigma
               else b
             in
             List.map (replace o#get_sigma) (List.rev xsb)
           in
           let o = o#restore envs in
           `Fun (fb, (tyvars, xsb, body), z, loc), o
        | `Rec defs ->
           let envs = o#backup in
           let o =
             List.fold_left (fun o (fb,_,_,_) -> snd (o#binder fb)) o defs
           in
           let (defs, o) =
             List.fold_left
               (fun (defs, o) (fb, (tyvars, (xsb : binder list), body), z, loc) ->
                 let o = o#reset in
                 let ((xsb : binder list), o) =
                   List.fold_left
                     (fun (bs, o) b ->
                       let (b, o) = o#binder b in
                       (b :: bs, o#add_parameter b))
                     ([], o) xsb
                 in
                 let (z, o) =
                   match z with
                   | None -> z, o
                   | Some z -> let (z, o) = o#binder z in Some z, o
                 in
                 let (body, _, o) = o#computation body in
                 let (fb, o) = o#binder fb in
                 let xsb =
                   let replace sigma b =
                     let var = Var.var_of_binder b in
                     if IntMap.mem var sigma then IntMap.find var sigma
                     else b
                   in
                   List.map (replace o#get_sigma) (List.rev xsb)
                 in
                 let def = (fb, (tyvars, xsb, body), z, loc) in
                 (def :: defs, o))
               ([], o) defs
           in
           let o = o#restore envs in
           `Rec (List.rev defs), o
        | e -> super#binding e

        method! bindings = function
        | [] -> [], o
        | `Let (b, (tyvars, tc)) :: bs ->
           begin match tc with
           | `Return v    when o#is_function_parameter v ->
              let var = o#get_variable v in
              let o = o#replace var b in
              let (_, o) = o#binder b in
              let (bs, o) = o#bindings bs in
              (bs, o)
           | _ ->
              let (b, o) = super#binding (`Let (b, (tyvars, tc))) in
              let (bs, o) = o#bindings bs in
              (b :: bs, o)
           end
        | b :: bs ->
           let (b, o) = o#binding b in
           let (bs, o) = o#bindings bs in
           (b :: bs, o)
      end
    in
    fst3 ((eliminator tyenv)#program prog)

end
