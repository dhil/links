open Utility
type comp_unit = Ir.program Js.comp_unit
type prog_unit = Js.program Js.comp_unit

(* Environment *)
module VEnv = Env.Int
type venv = string VEnv.t
  [@@deriving show]

let string_of_venv (env : venv) =
  let strings =
    Env.Int.fold
      (fun k v acc ->
        (Printf.sprintf "%d -> %s" k v) :: acc)
      env []
  in
  List.fold_left (fun acc str -> Printf.sprintf "%s%s\n" acc str) "" strings

let string_of_nenv (env : int Env.String.t) =
  let strings =
    Env.String.fold
      (fun k v acc ->
        (Printf.sprintf "%s -> %d" k v) :: acc)
      env []
  in
  List.fold_left (fun acc str -> Printf.sprintf "%s%s\n" acc str) "" strings

let string_of_liveness_map nenv (lm : IntSet.t IntMap.t) =
  let strings =
    IntMap.fold
      (fun k vs acc ->
        let k = Env.Int.lookup nenv k in
        (Printf.sprintf "%s -> %s" k (IntSet.show vs)) :: acc)
      lm []
  in
  List.fold_left (fun acc str -> Printf.sprintf "%s%s\n" acc str) "" strings


let initialise_envs (nenv, tyenv) =
  let dt = DesugarDatatypes.read ~aliases:tyenv.Types.tycon_env in

  (* TODO:

     - add stringifyB64 to lib.ml as a built-in function?
     - get rid of ConcatMap here?
  *)
    let tyenv =
      {Types.var_env =
          Env.String.bind
            (Env.String.bind tyenv.Types.var_env
               ("ConcatMap", dt "((a) -> [b], [a]) -> [b]"))
            ("stringifyB64", dt "(a) -> String");
       Types.tycon_env = tyenv.Types.tycon_env;
       Types.effect_row = tyenv.Types.effect_row } in
    let nenv =
      Env.String.bind
        (Env.String.bind nenv
           ("ConcatMap", Var.fresh_raw_var ()))
        ("stringifyB64", Var.fresh_raw_var ()) in

    let venv =
      Env.String.fold
        (fun name v venv -> VEnv.bind venv (v, name))
        nenv
        VEnv.empty in
    let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
    (nenv, venv, tenv)

(* Primitive operations *)
module type PRIM_DESC = sig
  val prim_desc : string -> string * int
end

module Primitives(P : PRIM_DESC) = struct
  let is : string -> bool
    = fun x ->
      try ignore (P.prim_desc x); true with
      | Not_found -> false

  let gen : op:string -> ?args:Js.expression list -> unit -> Js.expression
    = fun ~op ?(args=[]) () ->
      try
        let open Js in
        EApply (EPrim (fst @@ P.prim_desc op), args)
      with Not_found -> raise Not_found

  let prim_name : op:string -> unit -> string
    = fun ~op () ->
      try
        fst (P.prim_desc op)
      with Not_found -> raise Not_found

  let arity : op:string -> unit -> int
    = fun ~op () ->
      try
        snd (P.prim_desc op)
      with Not_found -> raise Not_found
end

module Prim_Arithmetic : PRIM_DESC = struct
  let prim_desc = function
    | "+" -> "%int_add",2  | "+." -> "%float_add",2
    | "-"  -> "%int_sub",2 | "-." -> "%float_sub",2
    | "*" -> "%int_mult",2 | "*." -> "%float_mult",2
    | "/" -> "%Number.integerDivision",2  | "/." -> "%float_div",2
    | "^" -> "%int_pow",2  | "^." -> "%float_pow",2
    | "mod" -> "%mod",2
    | _ -> raise Not_found
end

module Prim_String : PRIM_DESC = struct
  let prim_desc = function
    | "^^" -> "%String.concat",2
    | _ -> raise Not_found
end

module Prim_Comparison : PRIM_DESC = struct
  let prim_desc = function
    | "==" -> "%eq",2 | "<>" -> "%neq",2
    | "<"  -> "%lt",2 | ">"  -> "%gt",2
    | "<=" -> "%le",2 | ">=" -> "%ge",2
    | _ -> raise Not_found
end

module Prim_Functions : PRIM_DESC = struct
  let prim_desc = function
    | "random" -> "Math.random",1
    | "debug" -> "%IO.debug",1
    | "not"   -> "%not",1
    | "negatef" | "negate" -> "%negate",1
    | "Cons"  -> "%List.cons",2 | "tl" -> "%List.tail",1  | "hd" -> "%List.head",1
    | "length" -> "%List.length", 1
    | "Concat" -> "%List.concat",2
    | "error"  -> "%IO.error",1
    | "print"  -> "%IO.print",1
    | "perfNow"    -> "%Performance.now",0
    | "perfElapsed" -> "%Performance.elapsed", 2
    | "intToString" | "floatToString" -> "%String.ofNumber",1
    | "floatToInt" -> "%Number.toInt",1 | "intToFloat" -> "%Number.toFloat",1
    | "makeArray" -> "%Array.make",2
    | "arrayGet" -> "%Array.get", 2
    | "arraySet" -> "%Array.set", 3
    | "arrayLength" -> "%Array.length", 1
    | "sqrt" -> "Math.sqrt", 1
    | _ -> raise Not_found
end

module CPSFunctions = struct
  let prim_desc p =
    let name, arity = Prim_Functions.prim_desc p in
    match String.split_on_char '.' name with
    | ["%List"; "head"] -> "%ListCPS.head", arity
    | ["%List"; "tail"] -> "%ListCPS.tail", arity
    | ["%Array"; "get"] -> "%ArrayCPS.get", arity
    | ["%Array"; "set"] -> "%ArrayCPS.set", arity
    | ["%Array"; "make"] -> "%ArrayCPS.make", arity
    | _ -> name, arity

  let is : string -> bool
    = fun x ->
      try ignore (prim_desc x); true with
      | Not_found -> false

  let gen : string -> Js.expression list -> Js.expression -> Js.expression
    = fun op args kappa ->
      try
        let open Js in
        EApply (EPrim (fst @@ prim_desc op), args @ [kappa])
      with Not_found -> raise Not_found

  let prim_name : string -> string
    = fun op ->
      try
        fst (prim_desc op)
      with Not_found -> raise Not_found

  let arity : string -> int
    = fun op ->
      try
        snd (prim_desc op)
      with Not_found -> raise Not_found
end

module StringOp = Primitives(Prim_String)
module Arithmetic = Primitives(Prim_Arithmetic)
module Comparison = Primitives(Prim_Comparison)
module Functions  = Primitives(Prim_Functions)

(* Js compilers *)
let make_dictionary : (string * Js.expression) list -> Js.expression
  = fun fields -> Js.EObj fields

let make_array : Js.expression list -> Js.expression
  = fun elements ->
    Js.EArray (Array.of_list elements)

let strlit : Ir.name -> Js.expression
  = fun s -> Js.(ELit (LString s))

(* strip any top level polymorphism from an expression *)
let rec strip_poly = function
  | Ir.TAbs (_, e)
  | Ir.TApp (e, _) -> strip_poly e
  | e -> e

(** Generate a JavaScript name from a binder, wordifying symbolic names *)
let safe_name_binder (x, info) =
  let name = Js.name_binder (x, info) in
  if (name = "") then
    prerr_endline (Ir.show_binder (x, info))
  else
    ();
  assert (name <> "");
  (x, Js.name_binder (x,info))

module type JS_COMPILER = sig
  val compile : comp_unit -> prog_unit
  (* val compile_prelude : Ir.binding list Js.prelude_unit -> Js.decl list Js.prelude_unit *)
end

(* Higher-order CPS compiler *)
module CPS = struct
  let __kappa = Js.Ident.of_string "__kappa"
  let join_scopes : (Js.program -> Js.program) -> (Js.program -> Js.program) -> (Js.statement -> Js.program)
    = fun outer_scope inner_scope stmt -> outer_scope (inner_scope ([], stmt))
      (* fun stmt -> let (decls', stmt') = outer_scope ([], stmt) in *)
      (*             let (decls'', stmt'') = inner_scope stmt' in *)
      (*             (decls' @ decls''), stmt'' *)
  module K: sig
  (* Invariant: the continuation structure is algebraic. For
     programming purposes it is instructive to think of a continuation
     as an abstract list. *)
    type t

    val toplevel : t
  (* A continuation is a monoid. *)
    val identity : t
    val (<>) : t -> t -> t

  (* Returns a scope in which the head and tail of the continuation
     are accessible. *)
    val pop : t -> (Js.program -> Js.program) * t * t

  (* Turns code into a continuation. *)
    val reflect : Js.expression -> t
  (* Turns a continuation into code. *)
    val reify   : t -> Js.expression

  (* Continuation name binding. *)
    val bind : t -> (t -> Js.program) -> Js.program

  (* Continuation application generation. The optional strategy
     parameter decides whether the application should be yielding or
     direct. *)
    val apply : t -> Js.expression -> Js.expression

  (* Augments a function [Fn] with a continuation parameter and
     reflects the result as a continuation. The continuation parameter
     in the callback provides access to the current continuation. *)
    val contify_with_env : (t -> venv * Js.expression) -> venv * t
    end = struct
  (* We can think of this particular continuation structure as a
     nonempty stack with an even number of elements. *)
      type t = Cons of Js.expression * t
             | Reflect of Js.expression
             | Identity

  (* Auxiliary functions for manipulating the continuation stack *)
      let nil = Js.(EPrim "%List.nil")
      let cons x xs = Js.(EApply (EPrim "%List.cons", [x; xs]))
      let head xs = Js.(EApply (EPrim "%List.head", [xs]))
      let tail xs = Js.(EApply (EPrim "%List.tail", [xs]))
      let toplevel = Js.(Cons (EPrim "%K.pure", Cons (EPrim "%K.absurd", Reflect nil)))

      let reflect x = Reflect x
      let rec reify = function
        | Cons (v, vs) ->
           cons v (reify vs)
        | Reflect v -> v
        | Identity -> reify toplevel

      let identity = Identity
      let (<>) a b =
        match a,b with
        | Identity, b -> b
        | a, Identity -> a
        | Reflect ks, b -> Cons (ks, b)
        | Cons _ as a,b ->
           let rec append xs ys =
             match xs with
             | Cons (x, xs) -> Cons (x, append xs ys)
             | Reflect ks   -> Cons (ks, ys)
             | Identity     -> ys
           in
           append a b

      let bind kappas (body : t -> Js.program) =
        let open Js in
        let gen_binding : Js.Ident.t -> Js.expression -> Js.decl
          = fun b e ->
            DLet {
              bkind = `Const;
              binder = b;
              expr = e; }
        in
        let rec bind : Js.decl list -> (t -> Js.program) -> t -> Js.program
          = fun bs body -> function
          | Identity ->
             let k = Ident.make ~prefix:"_kappa" () in
             let (rest, stmt) = body (reflect (EVar k)) in
             let bs = (gen_binding k (reify Identity)) :: bs in
             bs @ rest, stmt
          | Reflect (EVar _) as k ->
             let (rest, stmt) = body k in
             bs @ rest, stmt
          | Reflect v ->
             let k = Ident.make ~prefix:"_kappa" () in
             let bs = (gen_binding k v) :: bs in
             let (rest, stmt) = body (reflect @@ EVar k) in
             bs @ rest, stmt
          | Cons ((EVar _) as v, kappas) ->
             bind bs (fun kappas -> body (Cons (v, kappas))) kappas
          | Cons (v, kappas) ->
             let k = Ident.make ~prefix:"_kappa" () in
             let bs = (gen_binding k v :: bs) in
             bind bs (fun kappas -> body (Cons (EVar k, kappas))) kappas
        in
        bind [] body kappas
        (* let rec bind (bs : Js.statement -> Js.program) (ks : t -> t) = *)
        (*   function *)
        (*   | Identity -> *)
        (*      let k = Ident.make ~prefix:"_kappa" () in *)
        (*      (fun code -> *)
        (*        let (decls, prog) = bs code in *)
        (*        (gen_bind k (reify Identity)) :: decls, code), ks, EVar k *)
        (*   | Reflect ((EVar _) as v) -> *)
        (*      bs, ks, v *)
        (*   | Reflect v -> *)
        (*      let k = Ident.make ~prefix:"_kappa" () in *)
        (*      (fun code -> let (decls, prog) = bs code in (gen_bind k v) :: decls, prog), ks, EVar k *)
        (*   | Cons ((EVar _) as v, kappas) -> *)
        (*      bind bs (fun kappas -> Cons (v, kappas)) kappas *)
        (*   | Cons (v, kappas) -> *)
        (*      let k = Ident.make ~prefix:"_kappa" () in *)
        (*      bind *)
        (*        (fun code -> let (decls, prog) = bs code in (gen_bind k v code) :: decls, prog) *)
        (*        (fun kappas -> Cons (EVar k, kappas)) kappas *)
        (* in *)
        (* let bs, ks, seed = bind (fun code -> code) (fun kappas -> kappas) kappas in *)
        (* bs (body (ks (reflect seed))) *)

      let apply k arg =
        Js.(EApply (EPrim "%K.apply", [reify k; arg]))

      let rec pop = function
        | Cons (kappa, kappas) ->
           (fun prog -> prog), (reflect kappa), kappas
        | Reflect ks ->
           let open Js in
           let __k = Ident.make ~prefix:"__k" () in
           let __ks = Ident.make ~prefix:"__ks" () in
           (fun (decls, stmt) ->
             let __k_binding =
               DLet {
                 bkind = `Const;
                 binder = __k;
                 expr = head ks; }
             in
             let __ks_binding =
               DLet {
                 bkind = `Const;
                 binder = __ks;
                 expr = tail ks; }
             in
             [__k_binding; __ks_binding] @ decls, stmt),
             (reflect (EVar __k)), reflect (EVar __ks)
        | Identity -> pop toplevel

      let contify_with_env fn =
        let open Js in
        let name = __kappa in
        match fn (reflect (EVar name)) with
        | env, EFun def -> env, reflect (EFun { def with formal_params = def.formal_params @ [name] })
        | _ -> failwith "error: contify: none function argument."
    end

  type continuation = K.t

  let contify fn =
    snd @@ K.contify_with_env (fun k -> VEnv.empty, fn k)

  let rec generate_value : venv -> Ir.value -> Js.expression
    = fun env ->
      let open Js in
      let open Utility in
      let open Ir in
      let gv v = generate_value env v in
      function
      | Constant c ->
         ELit (
           let open CommonTypes.Constant in
           match c with
           | Int v  -> LInt v
           | Float v  -> LFloat v
           | Bool v   -> LBool v
           | Char v   -> LChar v
           | String v -> LString v)
      | Variable var ->
       (* HACK *)
         let name = VEnv.lookup env var in
         if Arithmetic.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = [x; y; __kappa];
                  body = [], SReturn (K.apply (K.reflect (EVar __kappa)) (Arithmetic.gen ~op:name ~args:[EVar x; EVar y] ())) }
         else if StringOp.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = [x; y; __kappa];
                  body = [], SReturn (K.apply (K.reflect (EVar __kappa)) (StringOp.gen ~op:name ~args:[EVar x; EVar y] ())) }
         else if Comparison.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = [x; y; __kappa];
                  body = [], SReturn (K.apply (K.reflect (EVar __kappa)) (Comparison.gen ~op:name ~args:[EVar x; EVar y] ())) }
         else if Functions.is name then
           let rec replicate x = function
             | 0 -> []
             | n -> x :: (replicate x (n - 1))
           in
           let arity = Functions.arity ~op:name () in
           let formal_params = List.map (fun _ -> Ident.make ()) (replicate () arity) in
           let formal_params' = formal_params @ [__kappa] in
           let actual_params = List.map (fun i -> EVar i) formal_params in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = formal_params';
                  body = [], SReturn (K.apply (K.reflect (EVar __kappa)) (Functions.gen ~op:name ~args:actual_params ())) }
         else
           begin match name with
           | "Nil" -> EPrim "%List.nil"
           |  _ -> EVar name
           end
      | Extend (field_map, rest) ->
         let dict =
           make_dictionary
             (StringMap.fold
                (fun name v dict ->
                  (name, gv v) :: dict)
                field_map [])
         in
         begin
           match rest with
           | None -> dict
           | Some v ->
              EApply (EPrim "%Record.union", [gv v; dict])
         end
      | Project (name, v) ->
         EAccess (gv v, name)
      | Erase (names, v) ->
         EApply (EPrim "%Record.erase",
                 [gv v; make_array (List.map strlit (StringSet.elements names))])
      | Inject (name, v, _t) ->
         make_dictionary [("_label", strlit name); ("_value", gv v)]
    (* erase polymorphism *)
      | TAbs (_, v)
      | TApp (v, _) -> gv v
      | ApplyPure (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when StringOp.is f_name ->
                   StringOp.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when Comparison.is f_name ->
                   Comparison.gen ~op:f_name ~args:[gv l; gv r] ()
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     try
                       Functions.gen ~op:f_name ~args:(List.map gv vs) ()
                     with Not_found -> failwith (Printf.sprintf "Unsupported primitive (val): %s.\n" f_name)
                   else
                     EApply (gv (Variable f), (List.map gv vs))
              end
           | _ ->
              EApply (gv f, List.map gv vs)
         end
      | Closure (f, _, v) ->
         EApply (EPrim "%Closure.apply", [gv (Variable f); gv v])
      | Coerce (v, _) ->
         gv v
      | _ -> failwith "Unsupported value."

  and generate_tail_computation : venv -> Ir.tail_computation -> continuation -> Js.program
    = fun env tc kappa ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      let gc c kappa = snd (generate_computation env c kappa) in
      match tc with
      | Return v ->
         [], SReturn (K.apply kappa (gv v))
      | Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   [], SReturn (K.apply kappa (Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ()))
                | [l; r] when StringOp.is f_name ->
                   [], SReturn (K.apply kappa (StringOp.gen ~op:f_name ~args:[gv l; gv r] ()))
                | [l; r] when Comparison.is f_name ->
                   [], SReturn (K.apply kappa (Comparison.gen ~op:f_name ~args:[gv l; gv r] ()))
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     match f_name, vs with
                     | "deref", [v] ->
                        [], SReturn (K.apply kappa (EAccess (gv v, "_contents")))
                     | "ref", [v] ->
                        [], SReturn (K.apply kappa (EObj [("_contents", gv v)]))
                     | ":=", [r; v] ->
                        let destructive_update =
                          EApply (EPrim "%assign", [EAccess (gv r, "_contents"); gv v])
                        in
                        [], SSeq (SExpr destructive_update, SReturn (K.apply kappa (EObj [])))
                     | _ ->
                        let k = K.reify kappa in
                        let expr =
                          try
                            let args = List.map gv vs in
                            CPSFunctions.gen f_name args k
                          with Not_found -> failwith (Printf.sprintf "Unsupported primitive (tc): %s.\n" f_name)
                        in
                        [], SReturn expr
                   else
                     let k = K.reify kappa in
                     [], SReturn (EApply (gv (Variable f), (List.map gv vs) @ [k]))
              end
           | _ ->
              let k = K.reify kappa in
              [], SReturn (EApply (gv f, (List.map gv vs) @ [k]))
         end
      | Special special ->
         generate_special env special kappa
      | Case (v, cases, default) ->
         let v = gv v in
         let scrutineeb = Ident.make ~prefix:"_scrutinee" () in
         let bind_scrutinee scrutinee =
           DLet {
             bkind = `Const;
             binder = scrutineeb;
             expr = scrutinee; }
         in
         let open Utility in
         let (decls, prog) =
           K.bind kappa
             (fun kappa ->
               let translate_case (xb, c) =
                 let (x, x_name) = safe_name_binder xb in
                 let value_binding =
                   DLet { bkind = `Const;
                          binder = Ident.of_string x_name;
                          expr = EAccess (EVar scrutineeb, "_value"); }
                 in
                 let (_, (decls, stmt)) = generate_computation (VEnv.bind env (x, x_name)) c kappa in
                 value_binding :: decls, stmt
               in
               let cases = StringMap.map translate_case cases in
               let default = opt_map translate_case default in
               [], SCase (EAccess (EVar scrutineeb, "_label"), cases, default))
         in
         decls @ [bind_scrutinee v], prog
      | If (v, c1, c2) ->
         K.bind kappa
           (fun kappa ->
             [], SIf (gv v, gc c1 kappa, gc c2 kappa))

  and generate_special : venv -> Ir.special -> continuation -> Js.program
    = fun env sp kappa ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      match sp with
      | Wrong _ -> [], SReturn (EApply (EPrim "%error", [ELit (LString "Internal Error: Pattern matching failed")]))
      | DoOperation (name, args, _) ->
         let box = function
           | [v] -> gv v
           | vs -> make_dictionary (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let cons k ks =
           EApply (EPrim "%List.cons", [k;ks])
         in
         let nil = EPrim "%List.nil" in
         K.bind kappa
           (fun kappas ->
             let bind_skappa, skappa, kappas = K.pop kappas in
             let bind_seta, seta, kappas   = K.pop kappas in
             let bind = join_scopes bind_skappa bind_seta in
             let resumption = K.(cons (reify seta) (cons (reify skappa) nil)) in
             let op    =
               make_dictionary [ ("_label", strlit name)
                               ; ("_value", make_dictionary [("p", box args); ("s", resumption)]) ]
             in
             bind K.(SReturn (apply (seta <> kappas) op)))
      | Handle { Ir.ih_comp = comp; Ir.ih_return = return; Ir.ih_cases = eff_cases; Ir.ih_depth = depth } ->
         let open Utility in
         if depth = Shallow then
           failwith "Translation of shallow handlers has not yet been implemented.";
         (* Generate body *)
         let gb env binder body kappas =
           let env' = VEnv.bind env (safe_name_binder binder) in
           snd (generate_computation env' body kappas)
         in
         let (return_clause, operation_clauses) = (return, eff_cases) in
         let return =
           let (xb, body) = return_clause in
           let x_name = snd @@ safe_name_binder xb in
           contify (fun kappa ->
             let bind, _, kappa = K.pop kappa in
             EFun {
               fname = `Anonymous;
               fkind = `Regular;
               formal_params = [x_name];
               body = bind @@ gb env xb body kappa; })
         in
         let operations =
           (* Generate clause *)
           let gc env (xb, rb, body) kappas _z =
             let x_name = snd @@ safe_name_binder xb in
             let env', r_name =
               let rb' = safe_name_binder rb in
               VEnv.bind env rb', snd rb'
             in
             let v_name, v = Ident.make ~prefix:"_v" (), EAccess (EVar _z, "_value") in
             let p = EAccess (EVar v_name, "p") in
             let s = EAccess (EVar v_name, "s") in
             let r = EApply  (EPrim "%K.makeFun", [s]) in
             let clause_body =
               let v_binding =
                 DLet {
                   bkind = `Const;
                   binder = v_name;
                   expr = v; }
               in
               let r_binding =
                 DLet {
                   bkind = `Const;
                   binder = r_name;
                   expr = r; }
               in
               let p_binding =
                 DLet {
                   bkind = `Const;
                   binder = x_name;
                   expr = p; }
               in
               let (decls, stmt) = gb env' xb body kappas in
               v_binding :: r_binding :: p_binding :: decls, stmt
             in
             clause_body
           in
           let clauses kappas _z = StringMap.map (fun clause -> gc env clause kappas _z) operation_clauses in
           let forward ks _z =
             K.bind ks
               (fun ks ->
                 let bind1, k', ks' = K.pop ks in
                 let bind2, h', ks' = K.pop ks' in
                 let bind = join_scopes bind1 bind2 in
                 let resumption =
                   let s = Ident.of_string "s" in
                   let _x = Ident.make ~prefix:"_x" () in
                   let _x_binding =
                     DLet {
                       bkind = `Const;
                       binder = _x;
                       expr = EApply (EPrim "%List.cons", [K.reify k'; EVar s]); }
                   in
                   EFun {
                     fname = `Anonymous;
                     fkind = `Regular;
                     formal_params = [s];
                     body = [_x_binding], SReturn (EApply (EPrim "%List.cons", [K.reify h'; EVar _x]));
                   }
                 in
                 let vmap = EApply (EPrim "%K.vmap", [resumption; EVar _z]) in
                 bind (SReturn K.(apply (h' <> ks') vmap)))
           in
           let _z = Ident.of_string "_z" in
           contify
             (fun ks ->
               EFun {
                 fname = `Anonymous;
                 fkind = `Regular;
                 formal_params = [_z];
                 body = [], SCase (EAccess (EVar _z, "_label"),
                                   clauses ks _z,
                                   Some (forward ks _z)); })
         in
         let kappa = K.(return <> operations <> kappa) in
         let _, comp = generate_computation env comp kappa in
         comp
      | _ -> failwith "Unsupported special."

  and generate_computation : venv -> Ir.computation -> continuation -> venv * Js.program
    = fun env (bs, tc) kappa ->
      let open Js in
      let open Ir in
      let rec gbs : venv -> continuation -> Ir.binding list -> venv * Js.program =
        fun env kappa ->
          function
          | Let (b, (_, Return v)) :: bs ->
             let (x, x_name) = safe_name_binder b in
             let env', (rest, prog) = gbs (VEnv.bind env (x, x_name)) kappa bs in
             let x_binding =
               DLet {
                 bkind = `Const;
                 binder = Ident.of_string x_name;
                 expr = generate_value env v; }
             in
             (env', (x_binding :: rest, prog))
          | Let (b, (_, tc)) :: bs ->
             let (x, x_name) = safe_name_binder b in
             let x_name = Ident.of_string x_name in
             let bind, skappa, skappas = K.pop kappa in
             let env',skappa' =
               K.contify_with_env
                 (fun kappas ->
                   let env', body = gbs (VEnv.bind env (x, x_name)) K.(skappa <> kappas) bs in
                   env', EFun {
                     fname = `Anonymous;
                     fkind = `Regular;
                     formal_params = [x_name];
                     body; })
             in
             env', bind (generate_tail_computation env tc K.(skappa' <> skappas))
          | Fun ((fb, _, _zs, _location) as def) :: bs ->
             let (f, f_name) = safe_name_binder fb in
             let def_header = generate_function env [] def in
             let env', (rest, prog) = gbs (VEnv.bind env (f, f_name)) kappa bs in
             (env', (def_header :: rest, prog))
          | Rec defs :: bs ->
             let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) defs in
             let env', (rest, prog) = gbs (List.fold_left VEnv.bind env fs) kappa bs in
             let defs = List.map (generate_function env fs) defs in
             (env', (defs @ rest, prog))
          | Module _ :: bs -> gbs env kappa bs
          | Alien (bnd, raw_name, _lang) :: bs ->
             let (a, _a_name) = safe_name_binder bnd in
             let env' = VEnv.bind env (a, raw_name) in
             gbs env' kappa bs
          | [] -> (env, generate_tail_computation env tc kappa)
      in
      gbs env kappa bs

  and generate_program : venv -> Ir.program -> continuation -> venv * Js.program
    = fun env comp kappa ->
      let open Js in
      let venv,(decls,stmt) = generate_computation env comp kappa in
      let prog =
        match stmt with
        | SReturn e -> (decls, Js.SExpr e)
        | s -> (decls, s)
      in
      venv, prog
      (* let main = Ident.make ~prefix:"_main" () in *)
      (* let mainf = DFun { fname = `Named main; *)
      (*                   formal_params = []; *)
      (*                   body = prog } *)
      (* in *)
      (* venv, ([mainf], SExpr (EApply (EVar main, []))) *)

  and generate_toplevel_bindings : venv -> Ir.binding list -> continuation -> Js.decl list
    = fun env bs kappa ->
      let _,(decls,_) = generate_computation env (bs, Ir.Special (Ir.Wrong `Not_typed)) kappa in
      decls

  and generate_function : venv -> (Var.var * string) list -> Ir.fun_def -> Js.decl =
    fun env fs (fb, (_, xsb, body), zb, location) ->
      let open Js in
      let (_f, f_name) = safe_name_binder fb in
      assert (f_name <> "");
      (* prerr_endline ("f_name: "^f_name); *)
      (* optionally add an additional closure environment argument *)
      let xsb =
        match zb with
        | None -> xsb
        | Some zb -> zb :: xsb
      in
      let bs = List.map safe_name_binder xsb in
      let _xs, xs_names = List.split bs in
      let body_env = List.fold_left VEnv.bind env (fs @ bs) in
      let body =
        let open CommonTypes.Location in
        match location with
        | Client | Unknown ->
           snd (generate_computation body_env body (K.reflect (EVar __kappa)))
        | _ -> failwith "Only client side calls are supported."
      in
      DFun {
        fname = `Named (Ident.of_string f_name);
        fkind = `Regular;
        formal_params = (List.map Ident.of_string xs_names) @ [__kappa];
        body; }

  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let trampoline stmt =
        let cont =
          K.reflect
            (EApply
               (EVar "_List.cons",
                [EFun {
                  fkind = `Regular;
                  fname = `Anonymous;
                  formal_params = ["_x"; "_y"];
                  body = ([], stmt) };
                EApply
                  (EVar "_List.cons", [EVar "_K.absurd"; EVar "_List.nil"])]))
        in
        EApply
          (EVar "_Trampoline.run",
           [K.reify cont; EObj []])
      in
      let (_nenv, venv, _tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      (* Printf.printf "nenv:\n%s\n%!" (string_of_nenv u.envs.nenv); *)
      (* Printf.printf "venv:\n%s\n%!" (string_of_venv venv); *)
      let (_,prog) = generate_program venv u.program K.toplevel in
      let prog =
        match prog with
        | decls, stmt ->
           let mode =
             DLet {
               bkind = `Const;
               binder = Ident.of_string "_mode";
               expr = ELit (LString "CPS");
             }
           in
           mode :: decls, SExpr (trampoline stmt)
      in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["base.js"; "array.js"; "performance.js"; "cps.js"] in
      { u with program = prog; includes = u.includes @ dependencies }
end

(** Generator / Iterator compiler **)
module GenIter = struct

  let rec generate_program : venv -> Ir.program -> venv * Js.program
    = fun env (bs,tc) ->
      let open Js in
      let env', decls = generate_bindings ~toplevel:true env bs in
      let env'',body = generate_computation env' ([], tc) in
      let toplevel =
        SExpr (
          EApply
            (EPrim "%Toplevel.run",
             [EFun {
               fname = `Anonymous;
               fkind = `Generator;
               formal_params = [];
               body; }]))
      in
      env'', (decls, toplevel)

  and generate_computation : venv -> Ir.computation -> venv * Js.program
    = fun env (bs, tc) ->
      (* let open Js in *)
      let open Ir in
      let rec gbs : venv -> Ir.binding list -> venv * Js.program =
        fun env ->
          function
          | Module _ :: bs
          | Alien _ :: bs -> gbs env bs
          | b :: bs ->
             let env', decls = generate_binding ~toplevel:false env b in
             let (env'', (decls', stmt)) = gbs env' bs in
             env'', (decls @ decls', stmt)
          | [] -> (env, generate_tail_computation env tc)
      in
      gbs env bs

  and generate_bindings : ?toplevel:bool -> venv -> Ir.binding list -> venv * Js.decl list
    = fun ?(toplevel=false) env ->
      (* let open Js in *)
      let open Ir in
      let gbs env bs = generate_bindings ~toplevel env bs in
      function
      | Module _ :: bs -> gbs env bs
      | Alien (bnd, raw_name, _lang) :: bs ->
         let (a, _a_name) = safe_name_binder bnd in
         let env' = VEnv.bind env (a, raw_name) in
         gbs env' bs
      | b :: bs ->
         let env', decls = generate_binding ~toplevel env b in
         let (env'', decls') = gbs env' bs in
         env'', decls @ decls'
      | [] -> env, []

  and generate_binding : ?toplevel:bool -> venv -> Ir.binding -> venv * Js.decl list
    = fun ?(toplevel=false) env ->
      let open Js in
      let open Ir in
      (* let gv v = generate_value env v in *)
      function
      | Let (b, (_, Return v)) ->
         let (x, x_name) = safe_name_binder b in
         VEnv.bind env (x, x_name),
         [DLet {
           bkind = `Const;
           binder = Ident.of_string x_name;
           expr = generate_value env v; }]
      (* | `Let (b, (_, `Apply (f, args))) -> *)
      (*    let (x, x_name) = safe_name_binder b in *)
      (*    VEnv.bind env (x, x_name), *)
      (*    [DLet { *)
      (*      bkind = `Const; *)
      (*      binder = Ident.of_string x_name; *)
      (*      expr = EYield { ykind = `Star; *)
      (*                      yexpr = EApply (gv (strip_poly f), List.map gv args); }; }] *)
      | Let (b, (_, tc)) when toplevel = false ->
         let (x, x_name) = safe_name_binder b in
         VEnv.bind env (x, x_name),
         begin match generate_tail_computation env tc with
         | [], SReturn expr
         | [], SExpr expr ->
            [DLet {
              bkind = `Const;
              binder = Ident.of_string x_name;
              expr }]
         | body ->
            [DLet {
              bkind = `Const;
              binder = Ident.of_string x_name;
              expr =
                EYield ({ ykind = `Star;
                          yexpr = EApply (EFun {
                            fname = `Anonymous;
                            fkind = `Generator;
                            formal_params = [];
                            body;
                          }, []); }) }]
         end
      | Let (b, (_, tc)) ->
         let (x, x_name) = safe_name_binder b in
         let toplevel =
           DLet {
             bkind = `Const;
             binder = x_name;
             expr =
               EApply
                 (EPrim "%Toplevel.run",
                  [EFun {
                    fname = `Anonymous;
                    fkind = `Generator;
                    formal_params = [];
                    body = generate_tail_computation env tc; }]); }
         in
         VEnv.bind env (x, x_name),
         [toplevel]
      | Fun ((fb, _, _zs, _location) as def) ->
         let (f, f_name) = safe_name_binder fb in
         VEnv.bind env (f, f_name),
         [generate_function env [] def]
      | Rec defs ->
         let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) defs in
         let env' = List.fold_left VEnv.bind env fs in
         let defs = List.map (generate_function env fs) defs in
         env', defs
      | Module _  | Alien _ -> assert false

  and generate_tail_computation : venv -> Ir.tail_computation -> Js.program
    = fun env tc ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      let gc c = snd (generate_computation env c) in
      match tc with
      | Return v ->
         [], SReturn (gv v)
      | Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   [], SReturn (Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ())
                | [l; r] when StringOp.is f_name ->
                   [], SReturn (StringOp.gen ~op:f_name ~args:[gv l; gv r] ())
                | [l; r] when Comparison.is f_name ->
                   [], SReturn (Comparison.gen ~op:f_name ~args:[gv l; gv r] ())
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     match f_name, vs with
                     | "deref", [v] ->
                        [], SReturn (EAccess (gv v, "_contents"))
                     | "ref", [v] ->
                        [], SReturn (EObj [("_contents", gv v)])
                     | ":=", [r; v] ->
                        let destructive_update =
                          EApply (EPrim "%assign", [EAccess (gv r, "_contents"); gv v])
                        in
                        [], SSeq (SExpr destructive_update, SReturn (EObj []))
                     | _ ->
                        let expr =
                          try
                            let args = List.map gv vs in
                            Functions.gen ~op:f_name ~args ()
                          with Not_found -> failwith (Printf.sprintf "Unsupported primitive (tc): %s.\n" f_name)
                        in
                        [], SReturn expr (* SExpr (EYield { ykind = `Star; yexpr = expr; }) *)
                   else
                     (* let x_name = Ident.make ~prefix:"_x" () in *)
                     (* let x_binding = *)
                     (*   DLet { *)
                     (*     bkind = `Const; *)
                     (*     binder = x_name; *)
                     (*     expr = EYield { ykind = `Star; yexpr = EApply (gv (`Variable f), List.map gv vs) }; *)
                     (*   } *)
                     (* in *)
                     [], SReturn (EYield { ykind = `Star; yexpr = EApply (gv (Variable f), List.map gv vs) })
                     (* [x_binding], SReturn (EVar x_name) *)
              end
           | _ ->
              [], SReturn (EYield { ykind = `Star; yexpr = EApply (gv f, List.map gv vs) })
         end
      | Special special ->
         generate_special env special
      | Case (v, cases, default) ->
         let v = gv v in
         let scrutineeb = Ident.make ~prefix:"_scrutinee" () in
         let bind_scrutinee scrutinee =
           DLet {
             bkind = `Const;
             binder = scrutineeb;
             expr = scrutinee; }
         in
         let open Utility in
         let (decls, prog) =
           let translate_case (xb, c) =
             let (x, x_name) = safe_name_binder xb in
             let value_binding =
               DLet { bkind = `Const;
                      binder = Ident.of_string x_name;
                      expr = EAccess (EVar scrutineeb, "_value"); }
             in
             let (_, (decls, stmt)) = generate_computation (VEnv.bind env (x, x_name)) c in
             value_binding :: decls, stmt
           in
           let cases = StringMap.map translate_case cases in
           let default = opt_map translate_case default in
           [], SCase (EAccess (EVar scrutineeb, "_label"), cases, default)
         in
         decls @ [bind_scrutinee v], prog
      | If (v, c1, c2) ->
         [], SIf (gv v, gc c1, gc c2)
  and generate_special : venv -> Ir.special -> Js.program
    = fun env sp ->
      let open Ir in
      let open Js in
      let gv v = generate_value env v in
      match sp with
      | Wrong _ -> [], SReturn (EApply (EPrim "%error", [ELit (LString "Internal Error: Pattern matching failed")]))
      | DoOperation (name, args, _) ->
         let box = function
           | [v] -> gv v
           | vs -> make_dictionary (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let op =
           make_dictionary [ ("_label", strlit name)
                           ; ("_value", make_dictionary [("p", box args)]) ]
         in
         [], SExpr (EYield { ykind = `Regular; yexpr = op })
      | Handle { ih_comp; ih_return; ih_cases; ih_depth } ->
         let open Utility in
         let next iterator arg =
           match arg with
           | Some arg -> EApply (EAccess (iterator, "next"), [arg])
           | None -> EApply (EAccess (iterator, "next"), [])
         in
         let handle_next handle iterator arg =
           EApply (handle, [next iterator arg])
         in
         let make_shallow_resumption iterator =
           let x = Ident.of_string "x" in
           let expr = next iterator (Some (EVar x)) in
           EFun {
             fname = `Anonymous;
             fkind = `Generator;
             formal_params = [x];
             body = [], SReturn expr;
           }
         in
         let make_deep_resumption handle_name iterator =
           let x = Ident.of_string "x" in
           let yexpr = handle_next handle_name iterator (Some (EVar x)) in
           EFun {
             fname = `Anonymous;
             fkind = `Generator;
             formal_params = [x];
             body = [], SReturn (EYield { ykind = `Star; yexpr; });
           }
         in
         let generate_operation_clause env op_value resumption (xb, resumeb, body) =
           (* Generate arguments and resumption binders *)
           let (_, x_name) as xb = safe_name_binder xb in
           let p_binding =
             DLet {
               bkind = `Const;
               binder = x_name;
               expr = EAccess (EAccess (op_value, "_value"), "p");
             }
           in
           let env', bindings =
             let (_,r_name) as rb = safe_name_binder resumeb in
             let r_binding =
               DLet {
                 bkind = `Const;
                 binder = r_name;
                 expr = resumption;
               }
             in
             VEnv.bind (VEnv.bind env xb) rb, r_binding :: p_binding :: []
           in
           (* Generate the body *)
           let _, (decls, prog) = generate_computation env' body in
           bindings @ decls, prog
         in
         let generate_return_clause env value (xb, body) =
           let (x_binder, x_name) = safe_name_binder xb in
           let return_value_binding =
             DLet {
               bkind = `Const;
               binder = x_name;
               expr = value;
             }
             in
           let (_,(decls, stmt)) = generate_computation (VEnv.bind env (x_binder, x_name)) body in
           return_value_binding :: decls, stmt
         in
         (* Equality *)
         let (===) a b =
           EApply (EPrim "%eq", [a; b])
         in
         (* Deep handler compilation *)
         let deep_handle iterator =
           let op_or_value = Ident.make ~prefix:"_op_or_value" () in
           let handle_name = Ident.make ~prefix:"_handle" () in
           let return_clause, operation_clauses = (ih_return, ih_cases) in
           let op_value = EAccess (EVar op_or_value, "value") in
           (* Generate the return clause *)
           let return_clause =
             generate_return_clause env op_value return_clause
           in
           (* Generate the operation clauses *)
           let operation_clauses =
             let gc env clause =
               let resumption = make_deep_resumption (EVar handle_name) (EVar iterator) in
               generate_operation_clause env op_value resumption clause
             in
             (* Generate forwarding clause *)
             let forward =
               let x_name = Ident.make ~prefix:"_x" () in
               let x_binding =
                 DLet {
                   bkind = `Const;
                   binder = x_name;
                   expr = EYield { ykind = `Regular; yexpr = op_value };
                 }
               in
               [x_binding], SReturn (EYield { ykind = `Star; yexpr = handle_next (EVar handle_name) (EVar iterator) (Some (EVar x_name)) })
             in
             let scrutinee =
               EAccess (op_value, "_label")
             in
             [], SCase (scrutinee, StringMap.map (gc env) operation_clauses, Some forward)
           in
           (* Generate the handle *)
           handle_name, DFun {
             fkind = `Generator;
             fname = `Named handle_name;
             formal_params = [op_or_value];
             body =
               [], SIf ( (EAccess (EVar op_or_value, "done")) === (ELit (LBool true)),
                         return_clause,
                         operation_clauses);
           }
         in
         (* Shallow handler compilation *)
         let shallow_handle iterator =
         let op_or_value = Ident.make ~prefix:"_op_or_value" () in
           let return_clause, operation_clauses = (ih_return, ih_cases) in
           let op_value = EAccess (EVar op_or_value, "value") in
           (* Generate the return clause *)
           let return_clause =
             generate_return_clause env op_value return_clause
           in
           (* Generate the operation clauses *)
           let operation_clauses =
             let gc env clause =
               let resumption = make_shallow_resumption (EVar iterator) in
               generate_operation_clause env op_value resumption clause
             in
             (* Generate forwarding clause *)
             let forward =
               let x_name = Ident.make ~prefix:"_x" () in
               let x_binding =
                 DLet {
                   bkind = `Const;
                   binder = x_name;
                   expr = EYield { ykind = `Regular; yexpr = op_value };
                 }
               in
               let op_or_value_upd =
                 SAssign (op_or_value, next (EVar iterator) (Some (EVar x_name)))
               in
               [x_binding], SSeq (op_or_value_upd, SContinue)
             in
             let scrutinee =
               EAccess (op_value, "_label")
             in
             [], SCase (scrutinee, StringMap.map (gc env) operation_clauses, Some forward)
           in
           let op_or_value_binding =
             DLet {
               bkind = `Let;
               binder = op_or_value;
               expr = next (EVar iterator) None;
             }
           in
           (* Generate the handle *)
           let tt = ELit (LBool true) in
           [op_or_value_binding],
           SWhile (tt,
                   ([], SIf ( (EAccess (EVar op_or_value, "done")) === (ELit (LBool true)),
                              return_clause,
                              operation_clauses)))
         in
         (* Generate the iterator *)
         let iterator_name = Ident.make ~prefix:"_m" () in
         let wrapped_computation =
           let (_,body) = generate_computation env ih_comp in
           EApply
             (EFun {
               fkind = `Generator;
               fname = `Anonymous;
               formal_params = [];
               body; }, [])
         in
         let iterator_binding =
           DLet {
             bkind = `Const;
             binder = iterator_name;
             expr = wrapped_computation;
           }
         in
         (* Generate the appropriate handle *)
         begin match ih_depth with
         | Deep [] ->
            let handle_name, handle_fn = deep_handle iterator_name in
            let init = handle_next (EVar handle_name) (EVar iterator_name) None in
            [iterator_binding; handle_fn], SReturn (EYield { ykind = `Star; yexpr = init })
         | Deep _ -> failwith "Parameterised handlers are currently not supported."
         | Shallow ->
            let (bs, stmt) = shallow_handle iterator_name in
            iterator_binding :: bs, stmt
         end
      | _ -> failwith "Unsupported special."

  and generate_value : venv -> Ir.value -> Js.expression
    = fun env ->
      let open Js in
      let open Utility in
      let open Ir in
      let gv v = generate_value env v in
      function
      | Constant c ->
         ELit (
           let open CommonTypes.Constant in
           match c with
           | Int v  -> LInt v
           | Float v  -> LFloat v
           | Bool v   -> LBool v
           | Char v   -> LChar v
           | String v -> LString v)
      | Variable var ->
       (* HACK *)
         let name = VEnv.lookup env var in
         if Arithmetic.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = [x; y];
                  body = [], SReturn (Arithmetic.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if StringOp.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = [x; y];
                  body = [], SReturn (StringOp.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if Comparison.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = [x; y];
                  body = [], SReturn (Comparison.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if Functions.is name then
           let rec replicate x = function
             | 0 -> []
             | n -> x :: (replicate x (n - 1))
           in
           let arity = Functions.arity ~op:name () in
           let formal_params = List.map (fun _ -> Ident.make ()) (replicate () arity) in
           let actual_params = List.map (fun i -> EVar i) formal_params in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = formal_params;
                  body = [], SReturn (Functions.gen ~op:name ~args:actual_params ()) }
         else
           begin match name with
           | "Nil" -> EPrim "%List.nil"
           |  _ -> EVar name
           end
      | Extend (field_map, rest) ->
         let dict =
           make_dictionary
             (StringMap.fold
                (fun name v dict ->
                  (name, gv v) :: dict)
                field_map [])
         in
         begin
           match rest with
           | None -> dict
           | Some v ->
              EApply (EPrim "%Record.union", [gv v; dict])
         end
      | Project (name, v) ->
         EAccess (gv v, name)
      | Erase (names, v) ->
         EApply (EPrim "%Record.erase",
                 [gv v; make_array (List.map strlit (StringSet.elements names))])
      | Inject (name, v, _t) ->
         make_dictionary [("_label", strlit name); ("_value", gv v)]
    (* erase polymorphism *)
      | TAbs (_, v)
      | TApp (v, _) -> gv v
      | ApplyPure (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when StringOp.is f_name ->
                   StringOp.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when Comparison.is f_name ->
                   Comparison.gen ~op:f_name ~args:[gv l; gv r] ()
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     try
                       Functions.gen ~op:f_name ~args:(List.map gv vs) ()
                     with Not_found -> failwith (Printf.sprintf "Unsupported primitive (val): %s.\n" f_name)
                   else
                     EApply (gv (Variable f), (List.map gv vs))
              end
           | _ ->
              EApply (gv f, List.map gv vs)
         end
      | Closure (f, _, v) ->
         EApply (EPrim "%Closure.apply", [gv (Variable f); gv v]) (* The closure needs to be generator? *)
      | Coerce (v, _) ->
         gv v
      | _ -> failwith "Unsupported value."

  and generate_function : venv -> (Var.var * string) list -> Ir.fun_def -> Js.decl =
    fun env fs (fb, (_, xsb, body), zb, location) ->
      let open Js in
      let (_f, f_name) = safe_name_binder fb in
      assert (f_name <> "");
      (* prerr_endline ("f_name: "^f_name); *)
      (* optionally add an additional closure environment argument *)
      let xsb =
        match zb with
        | None -> xsb
        | Some zb -> zb :: xsb
      in
      let bs = List.map safe_name_binder xsb in
      let _xs, xs_names = List.split bs in
      let body_env = List.fold_left VEnv.bind env (fs @ bs) in
      let body =
        let open CommonTypes.Location in
        match location with
        | Client | Unknown ->
           snd (generate_computation body_env body)
        | _ -> failwith "Only client side calls are supported."
      in
      (* let body = *)
      (*   (\* HACK to ensure that *)
      (*      [[fun get() {do Get}]] *)
      (*     = *)
      (*      function* get() { return yield {"_label":"Get", "_value": ... } } *)
      (*   *\) *)
      (*   match body with *)
      (*   | (bs, SExpr ((EYield _) as yield)) -> *)
      (*      (bs, SReturn yield) *)
      (*   | _ -> body *)
      (* in *)
      DFun {
        fname = `Named (Ident.of_string f_name);
        fkind = `Generator;
        formal_params = (List.map Ident.of_string xs_names);
        body; }


  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let (_nenv, venv, tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      let prog = IrTraversals.EtaTailDos.program tenv u.program in
      let (_,prog) = generate_program venv prog in
      let prog =
        match prog with
        | decls, stmt ->
           let mode =
             DLet {
               bkind = `Const;
               binder = Ident.of_string "_mode";
               expr = ELit (LString "GENITER");
             }
           in
           mode :: decls, stmt
      in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["base.js"; "performance.js"; "geniter.js"] in
      { u with program = prog; includes = u.includes @ dependencies }
end

(* GenIter trampolined *)
                   (** Generator / Iterator compiler **)
module TrampolinedGenIter = struct

  let yield e =
    Js.(EYield { ykind = `Regular; yexpr = e; })

  let run f =
    Js.(EApply
          (EPrim "%CK.run", [f]))

  module Inst = struct
    open Js
    let inst = EVar "_Inst"
    let trap label argument =
      EApply (EAccess (inst, "trap"),  [label; argument])

    let set_trap_point handler =
      EApply (EAccess (inst, "setTrapPoint"), [handler])

    let return value =
      EApply (EAccess (inst, "value"), [value])

    let make_resumption =
      EApply (EAccess (inst, "bindResumption"), [])

    let forward op =
      EApply (EAccess (inst, "trap"), [EAccess (op, "label"); EAccess (op, "argument")])
  end

  let rec generate_program : venv -> Ir.program -> venv * Js.program
    = fun env (bs,tc) ->
      let open Js in
      let env', decls = generate_bindings ~toplevel:true env bs in
      let body = generate_tail_computation env' tc in
      let toplevel =
        let f =
          EFun {
              fkind = `Generator;
              fname = `Anonymous;
              formal_params = [];
              body
            }
        in
        SExpr (run f)
      in
      env', (decls, toplevel)

  and generate_computation : venv -> Ir.computation -> venv * Js.program
    = fun env (bs, tc) ->
    let env', decls = generate_bindings env bs in
    let (decls', stmt) = generate_tail_computation env' tc in
    env', (decls @ decls', stmt)

  and generate_bindings : ?toplevel:bool -> venv -> Ir.binding list -> venv * Js.decl list
    = fun ?(toplevel=false) env ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      let gbs env bs = generate_bindings ~toplevel env bs in
      function
      | Module _ :: bs -> gbs env bs
      | Alien (bnd, raw_name, _lang) :: bs ->
         let (a, _a_name) = safe_name_binder bnd in
         let env' = VEnv.bind env (a, raw_name) in
         gbs env' bs
      | Let (b, (_, tc)) :: bs ->
         let b = safe_name_binder b in
         let env', decls = gbs (VEnv.bind env b) bs in
         let expr =
           match tc with
           | Return v -> gv v
           | tc ->
              let body = generate_tail_computation env tc in
              let f =
                EFun {
                    fkind = `Generator;
                    fname = `Anonymous;
                    formal_params = [];
                    body }
              in
              if toplevel
              then run f
              else yield (EApply (f, []))
         in
         let decl =
           DLet {
               bkind = `Const;
               binder = snd b;
               expr }
         in
         env', decl :: decls
      | Fun ((fb, _, _zs, _location) as def) :: bs->
         let (f, f_name) = safe_name_binder fb in
         let env' = VEnv.bind env (f, f_name) in
         let decl = generate_function env [] def in
         let env'', decls = gbs env' bs in
         env'', decl :: decls
      | Rec defs :: bs->
         let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) defs in
         let env' = List.fold_left VEnv.bind env fs in
         let defs = List.map (generate_function env fs) defs in
         let env'', decls = gbs env' bs in
         env'', defs @ decls
      | [] -> env, []

  and generate_tail_computation : venv -> Ir.tail_computation -> Js.program
    = fun env tc ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      let gc c = snd (generate_computation env c) in
      match tc with
      | Return v ->
         [], SReturn (gv v)
      | Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   [], SReturn (Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ())
                | [l; r] when StringOp.is f_name ->
                   [], SReturn (StringOp.gen ~op:f_name ~args:[gv l; gv r] ())
                | [l; r] when Comparison.is f_name ->
                   [], SReturn (Comparison.gen ~op:f_name ~args:[gv l; gv r] ())
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     match f_name, vs with
                     | "deref", [v] ->
                        [], SReturn (EAccess (gv v, "_contents"))
                     | "ref", [v] ->
                        [], SReturn (EObj [("_contents", gv v)])
                     | ":=", [r; v] ->
                        let destructive_update =
                          EApply (EPrim "%assign", [EAccess (gv r, "_contents"); gv v])
                        in
                        [], SSeq (SExpr destructive_update, SReturn (EObj []))
                     | _ ->
                        let expr =
                          try
                            let args = List.map gv vs in
                            Functions.gen ~op:f_name ~args ()
                          with Not_found -> failwith (Printf.sprintf "Unsupported primitive (tc): %s.\n" f_name)
                        in
                        [], SReturn expr (* SExpr (EYield { ykind = `Star; yexpr = expr; }) *)
                   else
                     (* let x_name = Ident.make ~prefix:"_x" () in *)
                     (* let x_binding = *)
                     (*   DLet { *)
                     (*     bkind = `Const; *)
                     (*     binder = x_name; *)
                     (*     expr = EYield { ykind = `Star; yexpr = EApply (gv (`Variable f), List.map gv vs) }; *)
                     (*   } *)
                     (* in *)
                     [], SReturn (yield (EApply (gv (Variable f), List.map gv vs)))
                     (* [x_binding], SReturn (EVar x_name) *)
              end
           | _ ->
              [], SReturn (yield (EApply (gv f, List.map gv vs)))
         end
      | Special special ->
         generate_special env special
      | Case (v, cases, default) ->
         let v = gv v in
         let scrutineeb = Ident.make ~prefix:"_scrutinee" () in
         let bind_scrutinee scrutinee =
           DLet {
             bkind = `Const;
             binder = scrutineeb;
             expr = scrutinee; }
         in
         let open Utility in
         let (decls, prog) =
           let translate_case (xb, c) =
             let (x, x_name) = safe_name_binder xb in
             let value_binding =
               DLet { bkind = `Const;
                      binder = Ident.of_string x_name;
                      expr = EAccess (EVar scrutineeb, "_value"); }
             in
             let (_, (decls, stmt)) = generate_computation (VEnv.bind env (x, x_name)) c in
             value_binding :: decls, stmt
           in
           let cases = StringMap.map translate_case cases in
           let default = opt_map translate_case default in
           [], SCase (EAccess (EVar scrutineeb, "_label"), cases, default)
         in
         decls @ [bind_scrutinee v], prog
      | If (v, c1, c2) ->
         [], SIf (gv v, gc c1, gc c2)
  and generate_special : venv -> Ir.special -> Js.program
    = fun env sp ->
      let open Ir in
      let open Js in
      let gv v = generate_value env v in
      match sp with
      | Wrong _ -> [], SReturn (EApply (EPrim "%error", [ELit (LString "Internal Error: Pattern matching failed")]))
      | DoOperation (name, args, _) ->
         let box = function
           | [v] -> gv v
           | vs -> make_dictionary (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         [], SReturn (yield (Inst.trap (strlit name) (box args)))
      | Handle { ih_comp = m; ih_return = return; ih_cases = cases; ih_depth = depth } ->
         let open Utility in
         if depth = Shallow then failwith "Compilation of shallow handlers is currently not supported.";
         let f =
           EFun {
               fname = `Anonymous;
               fkind = `Generator;
               formal_params = [];
               body = snd (generate_computation env m)
             }
         in
         let return =
           let (xb, body) = return in
           let xb = safe_name_binder xb in
           let _, body = generate_computation (VEnv.bind env xb) body in
           EFun {
               fkind = `Generator;
               fname = `Anonymous;
               formal_params = [Ident.of_string (snd xb)];
               body
             }
         in
         let returnb = Ident.of_string (Utility.gensym ~prefix:"_return" ()) in
         let return_decl =
           DLet {
               bkind = `Const;
               binder = returnb;
               expr = return
             }
         in
         let eff =
           let case op (xb, rb, body) =
             let xb = safe_name_binder xb in
             let rb = safe_name_binder rb in
             let x_decl =
               DLet {
                   bkind = `Const;
                   binder = snd xb;
                   expr = EAccess (op, "argument") }
             in
             let resume_decl =
               DLet {
                   bkind = `Const;
                   binder = snd rb;
                   expr = yield Inst.make_resumption }
             in
             let env' = VEnv.bind (VEnv.bind env xb) rb in
             let _, (decls, stmt) = generate_computation env' body in
             x_decl :: resume_decl :: decls, stmt
           in
           let op = Ident.of_string "op" in
           let cases =
             StringMap.map (case (EVar op)) cases
           in
           let forward =
             [], SReturn (yield (Inst.forward (EVar op)))
           in
           let body = [], SCase (EAccess (EVar op, "label"),
                                 cases, Some forward)
           in
           EFun {
               fkind = `Generator;
               fname = `Anonymous;
               formal_params = [op];
               body }
         in
         let effb = Ident.of_string (Utility.gensym ~prefix:"_eff" ()) in
         let eff_decl =
           DLet {
               bkind = `Const;
               binder = effb;
               expr = eff
             }
         in
         let handle = EApply (EAccess (EVar "_Handle", "make"), [EVar returnb; EVar effb]) in
         let install = SExpr (yield (Inst.set_trap_point handle)) in
         [return_decl; eff_decl], SSeq (install, SReturn (yield (EApply (f, []))))
      | _ -> failwith "Unsupported special."

  and generate_value : venv -> Ir.value -> Js.expression
    = fun env ->
      let open Js in
      let open Utility in
      let open Ir in
      let gv v = generate_value env v in
      function
      | Constant c ->
         ELit (
           let open CommonTypes.Constant in
           match c with
           | Int v  -> LInt v
           | Float v  -> LFloat v
           | Bool v   -> LBool v
           | Char v   -> LChar v
           | String v -> LString v)
      | Variable var ->
       (* HACK *)
         let name = VEnv.lookup env var in
         if Arithmetic.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = [x; y];
                  body = [], SReturn (Arithmetic.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if StringOp.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = [x; y];
                  body = [], SReturn (StringOp.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if Comparison.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = [x; y];
                  body = [], SReturn (Comparison.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if Functions.is name then
           let rec replicate x = function
             | 0 -> []
             | n -> x :: (replicate x (n - 1))
           in
           let arity = Functions.arity ~op:name () in
           let formal_params = List.map (fun _ -> Ident.make ()) (replicate () arity) in
           let actual_params = List.map (fun i -> EVar i) formal_params in
           EFun { fname = `Anonymous;
                  fkind = `Generator;
                  formal_params = formal_params;
                  body = [], SReturn (Functions.gen ~op:name ~args:actual_params ()) }
         else
           begin match name with
           | "Nil" -> EPrim "%List.nil"
           |  _ -> EVar name
           end
      | Extend (field_map, rest) ->
         let dict =
           make_dictionary
             (StringMap.fold
                (fun name v dict ->
                  (name, gv v) :: dict)
                field_map [])
         in
         begin
           match rest with
           | None -> dict
           | Some v ->
              EApply (EPrim "%Record.union", [gv v; dict])
         end
      | Project (name, v) ->
         EAccess (gv v, name)
      | Erase (names, v) ->
         EApply (EPrim "%Record.erase",
                 [gv v; make_array (List.map strlit (StringSet.elements names))])
      | Inject (name, v, _t) ->
         make_dictionary [("_label", strlit name); ("_value", gv v)]
    (* erase polymorphism *)
      | TAbs (_, v)
      | TApp (v, _) -> gv v
      | ApplyPure (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when StringOp.is f_name ->
                   StringOp.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when Comparison.is f_name ->
                   Comparison.gen ~op:f_name ~args:[gv l; gv r] ()
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     try
                       Functions.gen ~op:f_name ~args:(List.map gv vs) ()
                     with Not_found -> failwith (Printf.sprintf "Unsupported primitive (val): %s.\n" f_name)
                   else
                     EApply (gv (Variable f), (List.map gv vs))
              end
           | _ ->
              EApply (gv f, List.map gv vs)
         end
      | Closure (f, _, v) ->
         EApply (EPrim "%Closure.apply", [gv (Variable f); gv v]) (* The closure needs to be generator? *)
      | Coerce (v, _) ->
         gv v
      | _ -> failwith "Unsupported value."

  and generate_function : venv -> (Var.var * string) list -> Ir.fun_def -> Js.decl =
    fun env fs (fb, (_, xsb, body), zb, location) ->
      let open Js in
      let (_f, f_name) = safe_name_binder fb in
      assert (f_name <> "");
      (* prerr_endline ("f_name: "^f_name); *)
      (* optionally add an additional closure environment argument *)
      let xsb =
        match zb with
        | None -> xsb
        | Some zb -> zb :: xsb
      in
      let bs = List.map safe_name_binder xsb in
      let _xs, xs_names = List.split bs in
      let body_env = List.fold_left VEnv.bind env (fs @ bs) in
      let body =
        let open CommonTypes.Location in
        match location with
        | Client | Unknown ->
           snd (generate_computation body_env body)
        | _ -> failwith "Only client side calls are supported."
      in
      (* let body = *)
      (*   (\* HACK to ensure that *)
      (*      [[fun get() {do Get}]] *)
      (*     = *)
      (*      function* get() { return yield {"_label":"Get", "_value": ... } } *)
      (*   *\) *)
      (*   match body with *)
      (*   | (bs, SExpr ((EYield _) as yield)) -> *)
      (*      (bs, SReturn yield) *)
      (*   | _ -> body *)
      (* in *)
      DFun {
        fname = `Named (Ident.of_string f_name);
        fkind = `Generator;
        formal_params = (List.map Ident.of_string xs_names);
        body; }


  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let (_nenv, venv, tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      let prog = IrTraversals.EtaTailDos.program tenv u.program in
      let (_,prog) = generate_program venv prog in
      let prog =
        match prog with
        | decls, stmt ->
           let mode =
             DLet {
               bkind = `Const;
               binder = Ident.of_string "_mode";
               expr = ELit (LString "GENITER");
             }
           in
           mode :: decls, stmt
      in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["base.js"; "performance.js"; "geniter.js"] in
      { u with program = prog; includes = u.includes @ dependencies }
end

(** CEK compiler **)
module CEK = struct

  module Make = struct
    type expr = Js.expression
    open Js

    let undefined = EVar "undefined"

    let ir_tag e =
      let label =
        match e with
        | `Shallow -> "SHALLOW"
        | `Deep -> "DEEP"
      in
      EAccess(EVar "IR", label)

    let ir : string -> expr list -> expr
      = fun fn_name -> function
      | [] -> EVar (Printf.sprintf "IR.Make.%s" fn_name) (* constant *)
      | args  ->  EApply (EVar (Printf.sprintf "IR.Make.%s" fn_name), args) (* function *)

    let computation : expr list -> expr -> expr
      = fun bindings tail_comp ->
        ir "computation" [EArray (Array.of_list bindings); tail_comp]

    let binding : string -> expr -> expr
      = fun name tail_comp ->
        ir "binding" [ELit (LString name); tail_comp]

    let variable : string -> expr
      = fun name ->
        ir "variable" [ELit (LString name)]

    let apply : expr -> expr list -> expr
      = fun f args ->
        ir "apply" [f; EArray (Array.of_list args)]

    let ifthenelse : expr -> expr -> expr -> expr
      = fun cond tt ff ->
        ir "ifthenelse" [cond; tt; ff]

    let casesplit : expr -> (string * expr) list -> expr option -> expr
      = fun scrutinee clauses default ->
        let open Utility in
        ir "casesplit" [scrutinee; EObj clauses; from_option undefined default]

    let clause : string -> expr -> expr =
      fun binder body ->
        ir "clause" [ELit (LString binder); body]

    let opclause : string -> string -> expr -> expr
      = fun parambinder resumebinder body ->
        ir "opclause" [ELit (LString parambinder); ELit (LString resumebinder); body]

    let handle : expr -> expr -> expr -> [`Deep of (string * expr) list | `Shallow] -> expr
      = fun comp ret ops -> function
      | `Deep params ->
         let params =
           List.map
             (fun (b,v) ->
               EObj [("binder", ELit (LString b)); ("initial_value", v)])
             params
         in
         ir "handle" [comp; ret; ops; ir_tag `Deep; EArray (Array.of_list params)]
      | `Shallow ->
        ir "handle" [comp; ret; ops; ir_tag `Shallow]

    let do_operation : string -> expr -> expr
      = fun label args ->
        ir "doOperation" [ELit (LString label); args]

    let constant : CommonTypes.Constant.t -> expr =
      let open CommonTypes.Constant in function
      | Int n    -> ir "constant" [ELit (LInt n)]
      | Float f  -> ir "constant" [ELit (LFloat f)]
      | Char c   -> ir "constant" [ELit (LChar c)]
      | Bool b   -> ir "constant" [ELit (LBool b)]
      | String s -> ir "constant" [ELit (LString s)]

    let return : expr -> expr
      = fun expr ->
        ir "ret" [expr]

    let fn : string -> string list -> string option -> expr -> expr
      = fun name params venv body ->
        let venv =
          match venv with
          | None -> undefined
          | Some z -> ELit (LString z)
        in
        let params = List.map (fun p -> ELit (LString p)) params in
        ir "fn" [ELit (LString name); EArray (Array.of_list params); venv; body]

    let inject : string -> expr -> expr
      = fun label args ->
        ir "inject" [ELit (LString label); args]

    let extend : (string * expr) list -> expr option -> expr
      = fun fields record ->
        let open Utility in
        ir "extend" [EObj fields; from_option undefined record]

    let project : expr -> string -> expr
      = fun record label ->
        ir "project" [record; ELit (LString label)]

    let closure : string -> expr -> expr
      = fun fn_name fvs ->
        ir "closure" [ELit (LString fn_name); fvs]

    let alien : string -> expr
      = fun fn_name ->
        ir "alien" [ELit (LString fn_name)]
  end

  let rec generate_program : venv -> Ir.program -> venv * Js.program
    = fun env (bs,tc) ->
      let open Js in
      let env', bindings, fenv, fun_decls = generate_toplevel_bindings env bs in
      let main = generate_tail_computation env' tc in
      let funsb = Ident.of_string "funs" in
      let env_of = Ident.of_string "Env.of" in
      let funs =
        DLet {
          bkind = `Const;
          binder = funsb;
          expr =
            EApply(EVar env_of, [EObj fenv]);
        }
      in
      let programb = Ident.of_string "program" in
      let program =
        DLet {
          bkind = `Const;
          binder = programb;
          expr = Make.computation bindings main;
        }
      in
      let resultb = Ident.of_string "result" in
      let value_to_string = Ident.of_string "Value.toString" in
      let cek_run = Ident.of_string "CEK.run" in
      let result =
        DLet {
          bkind = `Const;
          binder = resultb;
          expr =
            EApply (EVar value_to_string,
                    [EApply (EVar cek_run, [EVar funsb; EVar "Env.empty"; EVar programb])]);

        }
      in
      let toplevel =
        SExpr (
          EApply (EVar "_print", [EVar resultb]))
      in
      env', (fun_decls @ [funs; program; result], toplevel)

  and generate_toplevel_bindings : venv -> Ir.binding list -> venv * Js.expression list * (Js.label * Js.expression) list * Js.decl list
    = fun env bs ->
      let open Js in
      let open Ir in
      let fun_decl f_name f =
        DLet {
          bkind = `Const;
          binder = f_name;
          expr = f;
        }
      in
      let rec gbs env bindings fenv decls = function
        | [] -> env, List.rev bindings, fenv, List.rev decls
        | Module _ :: bs -> gbs env bindings fenv decls bs
        | Fun ((b,_,_,_) as fundef) :: bs ->
           let (fb, f_name) = safe_name_binder b in
           let f = generate_function env [] fundef in
           let decl =
             DLet {
               bkind = `Const;
               binder = f_name;
               expr = f;
             }
           in
           let env' = VEnv.bind env (fb, f_name) in
           gbs env' bindings ((f_name, EVar f_name) :: fenv) (decl :: decls) bs
        | Rec funs :: bs ->
         let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) funs in
         let env' = List.fold_left VEnv.bind env fs in
         let fenv', decls' =
           List.fold_left
             (fun (fenv, decls) ((fb, _, _, _) as def) ->
               let f = generate_function env' fs def in
               let f_name = VEnv.lookup env' (fst fb) in
               ((f_name, EVar f_name) :: fenv, (fun_decl f_name f) :: decls)
             ) (fenv, decls) funs
         in
         gbs env' bindings fenv' decls' bs
        | b :: bs -> (* Let or alien binding *)
           let env', binding = generate_binding env b in
           gbs env' (binding :: bindings) fenv decls bs
      in
      gbs env [] [] [] bs

  and generate_computation : venv -> Ir.computation -> venv * Js.expression
    = fun env (bs, tc) ->
    let env', bindings = generate_bindings env bs in
    let tail_comp = generate_tail_computation env' tc in
    env', Make.computation bindings tail_comp

  and generate_bindings : venv -> Ir.binding list -> venv * Js.expression list
    = fun env ->
    (* let open Js in *)
      let open Ir in
      let gbs env bs = generate_bindings env bs in
      function
      | Module _ :: bs -> gbs env bs
      | Fun _ :: bs | Rec _ :: bs -> gbs env bs (* assumed closure converted *)
      | b :: bs ->
         let env', expr = generate_binding env b in
         let (env'', expr') = gbs env' bs in
         env'',  expr :: expr'
      | [] -> env, []

  and generate_binding : venv -> Ir.binding -> venv * Js.expression
    = fun env ->
    let open Ir in function
    | Let (b, (_, tc)) ->
       let (b', bname) = safe_name_binder b in
       let env' = VEnv.bind env (b', bname) in
       env', Make.binding bname (generate_tail_computation env tc)
    | Alien (bnd, raw_name, _lang) ->
       let (a, a_name) = safe_name_binder bnd in
       let env' = VEnv.bind env (a, a_name) in
       env', Make.binding a_name (Make.return (Make.alien raw_name))
    | _ -> assert false

  (* and generate_binding : ?toplevel:bool -> venv -> Ir.binding -> venv * Js.decl list *)
  (*   = fun ?(toplevel=false) env -> *)
  (*     let open Js in *)
  (*     (\* let gv v = generate_value env v in *\) *)
  (*     function *)
  (*     | `Let (b, (_, `Return v)) -> *)
  (*        let (x, x_name) = safe_name_binder b in *)
  (*        VEnv.bind env (x, x_name), *)
  (*        [DLet { *)
  (*          bkind = `Const; *)
  (*          binder = Ident.of_string x_name; *)
  (*          expr = generate_value env v; }] *)
  (*     (\* | `Let (b, (_, `Apply (f, args))) -> *\) *)
  (*     (\*    let (x, x_name) = safe_name_binder b in *\) *)
  (*     (\*    VEnv.bind env (x, x_name), *\) *)
  (*     (\*    [DLet { *\) *)
  (*     (\*      bkind = `Const; *\) *)
  (*     (\*      binder = Ident.of_string x_name; *\) *)
  (*     (\*      expr = EYield { ykind = `Star; *\) *)
  (*     (\*                      yexpr = EApply (gv (strip_poly f), List.map gv args); }; }] *\) *)
  (*     | `Let (b, (_, tc)) when toplevel = false -> *)
  (*        let (x, x_name) = safe_name_binder b in *)
  (*        VEnv.bind env (x, x_name), *)
  (*        begin match generate_tail_computation env tc with *)
  (*        | [], SReturn expr *)
  (*        | [], SExpr expr -> *)
  (*           [DLet { *)
  (*             bkind = `Const; *)
  (*             binder = Ident.of_string x_name; *)
  (*             expr }] *)
  (*        | body -> *)
  (*           [DLet { *)
  (*             bkind = `Const; *)
  (*             binder = Ident.of_string x_name; *)
  (*             expr = *)
  (*               EYield ({ ykind = `Star; *)
  (*                         yexpr = EApply (EFun { *)
  (*                           fname = `Anonymous; *)
  (*                           fkind = `Generator; *)
  (*                           formal_params = []; *)
  (*                           body; *)
  (*                         }, []); }) }] *)
  (*        end *)
  (*     | `Let (b, (_, tc)) -> *)
  (*        let (x, x_name) = safe_name_binder b in *)
  (*        let toplevel = *)
  (*          DLet { *)
  (*            bkind = `Const; *)
  (*            binder = x_name; *)
  (*            expr = *)
  (*              EApply *)
  (*                (EPrim "%Toplevel.run", *)
  (*                 [EFun { *)
  (*                   fname = `Anonymous; *)
  (*                   fkind = `Generator; *)
  (*                   formal_params = []; *)
  (*                   body = generate_tail_computation env tc; }]); } *)
  (*        in *)
  (*        VEnv.bind env (x, x_name), *)
  (*        [toplevel] *)
  (*     | `Fun ((fb, _, _zs, _location) as def) -> *)
  (*        let (f, f_name) = safe_name_binder fb in *)
  (*        VEnv.bind env (f, f_name), *)
  (*        [generate_function env [] def] *)
  (*     | `Rec defs -> *)
  (*        let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) defs in *)
  (*        let env' = List.fold_left VEnv.bind env fs in *)
  (*        let defs = List.map (generate_function env fs) defs in *)
  (*        env', defs *)
  (*     | `Module _ | `Alien _ -> failwith "Module and Alien are unsupported." *)

  and generate_tail_computation : venv -> Ir.tail_computation -> Js.expression
    = fun env tc ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      let gc c = snd (generate_computation env c) in
      match tc with
      | Return v ->
         Make.return (gv v)
      | Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              let f =
                if Arithmetic.is f_name then
                  Make.variable (Arithmetic.prim_name ~op:f_name ())
                else if StringOp.is f_name then
                  Make.variable (StringOp.prim_name ~op:f_name ())
                else if Comparison.is f_name then
                  Make.variable (Comparison.prim_name ~op:f_name ())
                else if Functions.is f_name then
                  Make.variable (Functions.prim_name ~op:f_name ())
                else
                  if Lib.is_primitive f_name
                    && Lib.primitive_location f_name <> CommonTypes.Location.Server
                  then
                    match f_name with
                    | "deref" -> Make.variable "%Reference.deref"
                    | "ref" -> Make.variable "%Reference.ref"
                    | ":=" -> Make.variable "%Reference.assign"
                    | _ -> Make.variable f_name
                  else
                    Make.variable f_name
              in
              Make.apply f (List.map gv vs)
           | _ ->
              Make.apply (gv f) (List.map gv vs)
         end
      | Special special ->
         generate_special env special
      | Case (v, cases, default) ->
         let open Utility in
         let case (b, body) =
           let (b', bname) = safe_name_binder b in
           let env' = VEnv.bind env (b', bname) in
           Make.clause (Ident.of_binder b) (snd @@ generate_computation env' body)
         in
         let cases = StringMap.to_alist cases in
         let case' (label, (b, body)) =
           label, case (b, body)
         in
         Make.casesplit (gv v) (List.map case' cases) (opt_map case default)
      | If (v, c1, c2) ->
         Make.ifthenelse (gv v) (gc c1) (gc c2)
  and generate_special : venv -> Ir.special -> Js.expression
    = fun env sp ->
      let open Ir in
      let open Js in
      let gv v = generate_value env v in
      match sp with
      | Wrong _ ->
         Make.apply (EVar "%error") [ELit (LString "Internal Error: Pattern matching failed")]
      | DoOperation (name, args, _) ->
         let box = function
           | [v] -> gv v
           | vs -> Make.extend (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs) None
         in
         Make.do_operation name (box args)
      | Handle { ih_comp; ih_return; ih_cases; ih_depth } ->
         let open Utility in
         let _, m = generate_computation env ih_comp in
         let (return, operations) = (ih_return, ih_cases) in
         let op_case env (b, rb, body) =
           let env', rname =
             let (vresume, resume) = safe_name_binder rb in
             VEnv.bind env (vresume, resume), resume
           in
           let (b', bname) = safe_name_binder b in
           let env'' = VEnv.bind env' (b',bname) in
           Make.opclause (Ident.of_string bname) (Ident.of_string rname) (snd @@ generate_computation env'' body)
         in
         let return_case env (b, body) =
           let (b', bname) = safe_name_binder b in
           let env' = VEnv.bind env (b', bname) in
           Make.clause (Ident.of_string bname) (snd @@ generate_computation env' body)
         in
         let operations = StringMap.to_alist operations in
         let op_case' env (label, clause) =
           label, op_case env clause
         in
         let env, depth =
           match ih_depth with
           | Deep params ->
              let env, params =
                List.fold_right
                  (fun (b,v) (env, params) ->
                    let b' = safe_name_binder b in
                    let env' = VEnv.bind env b' in
                    let params' = (Ident.of_string (snd b'), gv v) :: params in
                    env', params')
                  params (env, [])
              in
              env, `Deep params
           | Shallow -> env, `Shallow
         in
         Make.handle m (return_case env return) (EObj (List.map (op_case' env) operations)) depth
      | _ -> failwith "Unsupported special."

  and generate_value : venv -> Ir.value -> Js.expression
    = fun env ->
      let open Ir in
      let gv v = generate_value env v in
      function
      | Constant c ->
         Make.constant c
      | Variable var ->
       (* HACK *)
         let name = VEnv.lookup env var in
         if Arithmetic.is name then
           Make.variable (Arithmetic.prim_name ~op:name ())
         else if StringOp.is name then
           Make.variable (StringOp.prim_name ~op:name ())
         else if Comparison.is name then
           Make.variable (Comparison.prim_name ~op:name ())
         else if Functions.is name then
           Make.variable (Functions.prim_name ~op:name ())
         else
           begin match name with
           | "Nil" -> Make.variable "%List.nil"
           |  _ -> Make.variable name
           end
      | Extend (field_map, rest) ->
         let open Utility in
         let fields =
           List.map
             (fun (l,v) -> (l, gv v))
             (StringMap.to_alist field_map)
         in
         Make.extend fields (opt_map gv rest)
      | Project (name, v) ->
         Make.project (gv v) name
      | Erase (_names, _v) ->
         failwith "Record erasure is unsupported."
      | Inject (name, v, _t) ->
         Make.inject name (gv v)
    (* erase polymorphism *)
      | TAbs (_, v)
      | TApp (v, _) -> gv v
      | ApplyPure (f, vs) ->
         generate_tail_computation env (Apply (f, vs))
      | Closure (f, _, v) ->
         let f_name = VEnv.lookup env f in
         Make.closure f_name (gv v)
      | Coerce (v, _) ->
         gv v
      | _ -> failwith "Unsupported value."

  and generate_function : venv -> (Var.var * string) list -> Ir.fun_def -> Js.expression =
    fun env fs (fb, (_, xsb, body), zb, location) ->
      let (_f, f_name) = safe_name_binder fb in
      assert (f_name <> "");
      (* prerr_endline ("f_name: "^f_name); *)
      (* optionally add an additional closure environment argument *)
      let body_env =
        let xsb =
          match zb with
          | None -> xsb
          | Some zb -> zb :: xsb
        in
        let bs = List.map safe_name_binder xsb in
        List.fold_left VEnv.bind env (fs @ bs)
      in
      let body =
        let open CommonTypes.Location in
        match location with
        | Client | Unknown ->
           snd (generate_computation body_env body)
        | _ -> failwith "Only client side calls are supported."
      in
      let xsb = List.map (fun x -> snd @@ safe_name_binder x) xsb in
      let zb = Utility.opt_map (fun x -> snd @@ safe_name_binder x) zb in
      Make.fn f_name xsb zb body

  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let (_nenv, venv, _tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      (* Printf.eprintf "nenv:\n%s\n%!" (string_of_nenv u.envs.nenv); *)
      (* Printf.eprintf "venv:\n%s\n%!" (string_of_venv venv); *)
      let (_,prog) = generate_program venv u.program in
      let prog =
        match prog with
        | decls, stmt ->
           let mode =
             DLet {
               bkind = `Const;
               binder = Ident.of_string "_mode";
               expr = ELit (LString "CEK");
             }
           in
           mode :: decls, stmt
      in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["performance.js"; "immutable.js"; "cek.js"] in
      { u with program = prog; includes = u.includes @ dependencies }
end

(* Generalised Stack Inspection compiler *)
module StackInspection = struct
  type liveset = IntSet.t
  and continuation_point = {
      before: liveset;
      after: liveset
    }
  and liveness_map = continuation_point IntMap.t
                       [@@deriving show]

  module ProcedureFragmentation = struct
    open Utility
    open Ir

    let liveness tyenv prog =
      let analysis tyenv =
        object (o)
          inherit IrTraversals.Iter.visitor(tyenv) as super

          val liveness_map = IntMap.empty  (* binder |-> before and after live sets *)
          val liveset = IntSet.empty       (* current live set being built *)

          method use var = (* registers a use of a variable, provided it is not primitive or a function binder *)
            if Lib.is_primitive_var var || Tables.mem Tables.fun_defs var then o
            else {< liveset = IntSet.add var liveset >}

          method kill b = (* removes a binder from the current live set *)
            let var = Var.var_of_binder b in
            {< liveset = IntSet.remove var liveset >}

          method with_liveset liveset = (* updates the current live set *)
            {< liveset = liveset >}
          method get_liveset = liveset

          method register b before after = (* registers a continuation point with its live sets *)
            let var = Var.var_of_binder b in
            {< liveness_map = IntMap.add var { before = before; after = after } liveness_map >}

          method get_liveness_map = liveness_map

          (* method! bindings bs = *)
          (*   o#list (fun o -> o#binding) (List.rev bs) *)

          method! binding = function
          | Let (b, (_, tc)) ->
             let after = o#get_liveset in
             let o = (o#with_liveset IntSet.empty)#tail_computation tc in (* computes use *)
             let use = o#get_liveset in
             let before = (* (use U after) - {b} *)
               IntSet.(remove (Var.var_of_binder b) (union_all [use; after]))
             in
             (o#with_liveset before)#register b before after
          | Fun (fb, (_, params, body), z, _) ->
             let o = (o#with_liveset IntSet.empty)#computation body in
             let after = IntSet.remove (Var.var_of_binder fb) o#get_liveset in
             let o = o#list (fun o -> o#kill) params in
             let o = o#option (fun o -> o#kill) z in
             let before = o#get_liveset in
             o#register fb before after
          | Rec fundefs ->
             o#list
               (fun o (fb, (_, params, body), z, _loc) ->
                 let o = (o#with_liveset IntSet.empty)#computation body in
                 let after = IntSet.remove (Var.var_of_binder fb) o#get_liveset in
                 let o = o#list (fun o -> o#kill) params in
                 let o = o#option (fun o -> o#kill) z in
                 let before = o#get_liveset in
                 o#register fb before after)
               fundefs
          | e -> super#binding e

          method! var var =
            (* Printf.eprintf "Var: %d\n%!" var; *)
            (* Printf.eprintf "Var %d is a fun? %s\n%!" var (if Tables.mem Tables.fun_defs var then "TRUE" else "FALSE"); *)
            o#use var

          method! tail_computation = function
          | If (cond, tt, ff) ->
             let o = o#computation tt in
             let after_tt = o#get_liveset in
             let o = o#computation ff in
             let after_ff = o#get_liveset in
             let before_if = IntSet.union_all [after_tt; after_ff] in
             Printf.eprintf "Liveset before if:%s\n%!" (show_liveset before_if);
             (o#with_liveset before_if)#value cond
          | Case (scrutinee, cases, default) ->
             let lss, o =
               StringMap.fold
                 (fun _ (xb, comp) (lss, o) ->
                   let o = o#binder xb in
                   let o = o#computation comp in
                   let o = o#kill xb in
                   let after = o#get_liveset in
                   (after :: lss, o))
                 cases ([], o)
             in
             let o, after_default =
               match default with
               | None -> o, IntSet.empty
               | Some (xb, comp) ->
                  let o = o#binder xb in
                  let o = o#computation comp in
                  let o = o#kill xb in
                  o, o#get_liveset
             in
             let after_cases = IntSet.union_all (after_default :: lss) in
             (o#with_liveset after_cases)#value scrutinee
          | e -> super#tail_computation e

          method! special = function
            | Handle { Ir.ih_comp = m; Ir.ih_return = return; Ir.ih_cases = cases; Ir.ih_depth = depth } ->
             let lss, o =
               StringMap.fold
                 (fun _ (xb, rb, comp) (lss, o) ->
                   let o = o#binder xb in
                   let o = o#binder rb in
                   let o = (o#with_liveset IntSet.empty)#computation comp in
                   let o = o#kill rb in
                   let o = o#kill xb in
                   let after = o#get_liveset in
                   let o = o#register xb IntSet.empty after in (* hack *)
                   (after :: lss, o))
                 cases ([], o)
             in
             let o, after_return =
               let (xb, comp) = return in
               let o = o#binder xb in
               let o = (o#with_liveset IntSet.empty)#computation comp in
               let o = o#kill xb in
               let ls = o#get_liveset in
               let o = o#register xb IntSet.empty ls in (* hack *)
               o, ls
             in
             let after_cases = IntSet.union_all (after_return :: lss) in
             let o, _after_comp =
               let o = (o#with_liveset after_cases)#computation m in
               o, o#get_liveset
             in
             begin match depth with
             | Shallow -> o
             | Deep params ->
                let (_lss, o) =
                  List.fold_left
                    (fun (lss, o) (b, v) ->
                      let o = (o#with_liveset IntSet.empty)#value v in
                      let o = o#binder b in
                      let o = o#kill b in
                      let before = o#get_liveset in
                      let o = o#register b before IntSet.empty in (* hack *)
                      (before :: lss, o))
                    ([], o) params
                in o
                     (* let after_params = IntSet.union_all *)
             end
            | e -> super#special e

          method! computation (bs, tc) =
            let o = (o#with_liveset IntSet.empty)#tail_computation tc in
            List.fold_right
              (fun b o -> o#binding b) bs o
        end
      in
      let o = (analysis tyenv)#program prog in
      o#get_liveness_map

    (* (\* Note this pass is not hygienic. Use with caution. *\) *)
    (* let alpha_convert tyenv renaming = *)
    (*   object (o) *)
    (*     inherit Ir.Transform.visitor(tyenv) as super *)

    (*     val renaming = renaming *)

    (*     method rename v = *)
    (*       try IntMap.find v renaming with *)
    (*       | NotFound _ -> v *)

    (*     method! var v = *)
    (*       let (v, dt, o) = super#var v in *)
    (*       (o#rename v, dt, o) *)

    (*   end *)

    (* let fragmentise (tyenv : Types.datatype Env.Int.t) prog = *)
    (*   let liveness_map = liveness tyenv prog in *)
    (*   (\* Printf.eprintf "Liveness_map: %s\n" (Show_liveness_map.show liveness_map); *\) *)
    (*   let counter = ref (-1) in *)
    (*   let fresh_answer_name base = *)
    (*     incr counter; *)
    (*     Printf.sprintf "_%s_an%d" base !counter *)
    (*   in *)
    (*   let freshen_binders bs = *)
    (*     let freshen_binder (binders, sigma) (var, info) = *)
    (*       let var' = Var.fresh_raw_var () in *)
    (*       ((var', info) :: binders, IntMap.add var var' sigma) *)
    (*     in *)
    (*     let (bs', sigma) = List.fold_left freshen_binder ([], IntMap.empty) bs in *)
    (*     List.rev bs', sigma *)
    (*   in *)
    (*   let fresh_answer_binder base : Ir.binder = *)
    (*     Var.(make_local_info ->- fresh_binder) (`Not_typed, fresh_answer_name base) *)
    (*   in *)
    (*   let arglist_of_liveset ls : Ir.value list = *)
    (*     List.map (fun (v : Ir.var) -> `Variable v) (IntSet.to_list ls) *)
    (*   in *)
    (*   let fragmentise = *)
    (*     object(o) *)
    (*       inherit Ir.Transform.visitor(tyenv) as super *)

    (*       val basename = "_toplevel" *)
    (*       method with_basename name = *)
    (*         {< basename = name >} *)
    (*       method binders_of_vars vars : Ir.binder list = *)
    (*         let binder_of_var v : Ir.binder = *)
    (*           let ty = Env.Int.lookup tyenv v in *)
    (*           (v, Var.info_of_type ty) *)
    (*         in *)
    (*         List.map binder_of_var vars *)

    (*       method refresh_binders bs = bs, IntMap.empty *)

    (*       method make_param_list liveset : Ir.binder list * Ir.var IntMap.t = *)
    (*         let xs = o#binders_of_vars (IntSet.to_list liveset) in *)
    (*         o#refresh_binders xs *)

    (*       val liveset = IntSet.empty *)
    (*       method with_liveset ls = *)
    (*         {< liveset = ls >} *)
    (*       method get_liveset = liveset *)

    (*       val answer_frames = IntSet.empty *)
    (*       method add_answer_frame b = *)
    (*         {< answer_frames = IntSet.add (Var.var_of_binder b) answer_frames >} *)
    (*       method get_answer_frames = answer_frames *)

    (*       method! computation (bs, tc) = *)
    (*         let split_bindings bs = *)
    (*           let is_let_binding = function | `Let _ -> true | _ -> false in *)
    (*           List.fold_right *)
    (*             (fun b (lets, other) -> *)
    (*               if is_let_binding b then (b :: lets, other) *)
    (*               else (lets, b :: other)) *)
    (*             bs ([], []) *)
    (*         in *)
    (*         let rec letbindings o liveset fb tc = function *)
    (*           | [] -> *)
    (*              let fundef : Ir.fun_def = *)
    (*                Printf.eprintf "Liveset at %s: %s\n%!" (Var.name_of_binder fb) (Show_liveset.show liveset); *)
    (*                let params, _sigma = o#make_param_list liveset in *)
    (*                let body = *)
    (*                  let tc = *)
    (*                    let (tc, _, _) = o#tail_computation tc in *)
    (*                    tc *)
    (*                    (\* fst3 ((alpha_convert tyenv sigma)#tail_computation tc) *\) *)
    (*                  in *)
    (*                  ([], tc) *)
    (*                in *)
    (*                (fb, ([], params, body), None, `Unknown) *)
    (*              in *)
    (*              [fundef], o *)
    (*           | (`Let (b, (tyvars, tc'))) :: bs -> *)
    (*              let (b, o) = o#binder b in *)
    (*              let var = Var.var_of_binder b in *)
    (*              let liveset = IntMap.find var liveness_map in *)
    (*              let f', fb' = *)
    (*                let b = fresh_answer_binder basename in *)
    (*                `Variable (fst b), b *)
    (*              in *)
    (*              let (_, o) = o#binder fb' in *)
    (*              let o = o#add_answer_frame fb' in *)
    (*              let fundefs, o = letbindings (o#with_liveset liveset.before) liveset.after fb' tc bs in *)
    (*              Printf.eprintf "Liveset at %d,%s: %s\n%!" var (Var.name_of_binder fb) (Show_continuation_point.show liveset); *)
    (*              let fundef : Ir.fun_def = *)
    (*                let params, _sigma = o#make_param_list liveset.before in *)
    (*                let args = arglist_of_liveset liveset.after in *)
    (*                let body : Ir.computation = *)
    (*                  let tc = *)
    (*                    let (tc, _, _) = (o#with_liveset liveset.before)#tail_computation tc' in *)
    (*                    tc *)
    (*                    (\* fst3 ((alpha_convert tyenv sigma)#tail_computation tc) *\) *)
    (*                  in *)
    (*                  [`Let (b, (tyvars, tc))], `Apply (f', args) *)
    (*                in *)
    (*                (fb, ([], params, body), None, `Unknown) *)
    (*              in *)
    (*              fundef :: fundefs, o *)
    (*           | _ -> assert false (\* Assumes closure conversion *\) *)
    (*         in *)
    (*         let funbinding o : Ir.binding -> Ir.binding * 'self_type = function *)
    (*           | `Fun (fb, (tyvars, (params : Ir.binder list), body), (z : Ir.binder option), loc) -> *)
    (*              let z, o = *)
    (*                match z with *)
    (*                | None -> z, o *)
    (*                | Some z -> let z, o = o#binder z in Some z, o *)
    (*              in *)
    (*              let ((params : Ir.binder list), o) = *)
    (*                List.fold_left *)
    (*                  (fun (ps, o) p -> *)
    (*                    let (p, o) = o#binder p in (p :: ps, o)) *)
    (*                  ([], o) params *)
    (*              in *)
    (*              let o = o#with_basename (Var.name_of_binder fb) in *)
    (*              let liveset = IntMap.find (Var.var_of_binder fb) liveness_map in *)
    (*              let (body, _, o) = (o#with_liveset liveset.after)#computation body in *)
    (*              let ((fb : Ir.binder), o) = o#binder fb in *)
    (*              (`Fun (fb, (tyvars, List.rev params, body), z, loc)), o *)
    (*           | `Rec fundefs -> *)
    (*              let o = *)
    (*                List.fold_right *)
    (*                  (fun (f, _, _, _) o -> *)
    (*                    let _, o = o#binder f in o) *)
    (*                  fundefs o *)
    (*              in *)
    (*              let defs, o = *)
    (*                List.fold_right *)
    (*                  (fun (fb, (tyvars, (params : Ir.binder list), body), (z : Ir.binder option), loc) (defs, o) -> *)
    (*                    let z, o = *)
    (*                      match z with *)
    (*                      | None -> z, o *)
    (*                      | Some z -> let z, o = o#binder z in Some z, o *)
    (*                    in *)
    (*                    let params, o = *)
    (*                      List.fold_right *)
    (*                        (fun p (ps, o) -> *)
    (*                          let p, o = o#binder p in *)
    (*                          (p :: ps, o)) *)
    (*                        params ([], o) *)
    (*                    in *)
    (*                    let o = o#with_basename (Var.name_of_binder fb) in *)
    (*                    let liveset = IntMap.find (Var.var_of_binder fb) liveness_map in *)
    (*                    let (body, _, o) = (o#with_liveset liveset.after)#computation body in *)
    (*                    ((fb, (tyvars, params, body), z, loc) :: defs, o)) *)
    (*                  fundefs ([], o) *)
    (*              in *)
    (*              `Rec defs, o#with_liveset IntSet.empty *)
    (*           | b -> b, o *)
    (*         in *)
    (*         let (lets, rest) = split_bindings bs in *)
    (*         let initial_f, (initial_fb : Ir.binder) = *)
    (*           let b = fresh_answer_binder basename in *)
    (*           `Variable (fst b), b *)
    (*         in *)
    (*         let (_, o) = o#binder initial_fb in *)
    (*         let o = o#add_answer_frame initial_fb in *)
    (*         let answer_frames, o = *)
    (*           letbindings o liveset initial_fb tc lets *)
    (*         in *)
    (*         let funs, o = *)
    (*           List.fold_left *)
    (*             (fun (bs, o) b -> *)
    (*               let b, o = funbinding o b in *)
    (*               (b :: bs, o)) *)
    (*             ([], o) rest *)
    (*         in *)
    (*         (funs @ [`Rec answer_frames], `Apply (initial_f, arglist_of_liveset o#get_liveset)), `Not_typed, o#with_liveset IntSet.empty *)

    (*       method! program comp = *)
    (*         let o = o#with_basename "_toplevel" in *)
    (*         o#computation comp *)
    (*     end *)
    (*   in *)
    (*   let (prog, _, o) = fragmentise#program prog in *)
    (*   prog, o#get_answer_frames *)

    let opt_fragmentise (tyenv : Types.datatype Env.Int.t) liveness_map prog =
      (* Printf.eprintf "Liveness_map: %s\n" (Show_liveness_map.show liveness_map); *)
      let frame_counter = ref 0 in
      let opt_fragmentise =
        object(o : 'self_type)
          inherit IrTraversals.Transform.visitor(tyenv)

          (* Frame binder generation *)
          val basename = "unknown"
          method with_basename name =
            {< basename = name >}
          method get_basename = basename

          (* val frame_counter = ref 0 *)
          (* method with_frame_counter fc = *)
          (*   {< frame_counter = ref fc >} *)
          method fresh_frame_binder ?var () =
            let var =
              match var with
              | None -> Var.fresh_raw_var ()
              | Some var -> var
            in
            let make_binder =
              Var.(make_local_info ->- fresh_binder)
            in
            incr frame_counter;
            let ty =
              Types.make_pure_function_type [`Not_typed] `Not_typed
            in
            make_binder (ty, Printf.sprintf "_%s_%d_an%d" basename var !frame_counter)

          method make_parameters pred vars : Ir.binder list =
            let binder_of_var v : Ir.binder =
              let ty = Env.Int.lookup tyenv v in
              (v, Var.info_of_type ty)
            in
            match pred with
            | None -> List.map binder_of_var vars
            | Some v' ->
               let vars = List.filter (fun v -> v <> v') vars in
               (binder_of_var v') :: List.map binder_of_var vars

          method make_arguments pred (vars : Ir.var list) : Ir.value list =
            match pred with
            | None -> List.map (fun v -> Variable v) vars
            | Some v' ->
               let vars = List.filter (fun v -> v <> v') vars in
               (Variable v') :: List.map (fun v -> Variable v) vars

          (* Keep track of which binders are frame binders *)
          val frame_binders = IntSet.empty
          method add_frame_binder b =
            {< frame_binders = IntSet.add (Var.var_of_binder b) frame_binders >}
          method get_frame_binders = frame_binders


          (* Explicit continuation stack for frame generation *)
          val continuation : Ir.tail_computation list = []
          method with_cont k =
            {< continuation = k >}
          method pop_cont =
            match continuation with
            | [] -> failwith "Empty continuation"
            | tc :: k ->
               tc, o#with_cont k
          method push_cont tc =
            o#with_cont (tc :: continuation)

          (* Current live set *)
          val liveset = IntSet.empty
          method with_liveset ls =
            {< liveset = ls >}
          method get_liveset = liveset

          (* Bookkeeping *)
          method backup = (continuation, basename, !frame_counter, liveset)
          method restore (k, basename, _fc, ls) =
            let o = o#with_cont k in
            (* let o = o#with_frame_counter fc in *)
            let o = o#with_basename basename in
            o#with_liveset ls
          method reset =
            let o = o#with_cont [] in
            (* let o = o#with_frame_counter 0 in *)
            let o = o#with_basename "unknown" in
            o#with_liveset IntSet.empty

          (* Convenient when visiting recursive functions *)
          method fun_binder (fb, _, _, _) =
            snd (o#binder fb)

          (* Now, the actual algorithm *)
          method! computation ((bs, tc) : Ir.computation) =
            (* the predecessor (pred) argument is a slight hack. It is
               the name of previous binding, if any. This must be the
               first argument of the continuation frame. *)
            let rec generate_frames (o : 'self_type) (pred : Ir.var option) : Ir.binding list -> (Ir.fun_def list * Ir.binding list * 'self_type) = function
              | [] -> [], [], o
              | [Let (b, (tyvars, tc))] ->
                 let var = Var.var_of_binder b in
                 let liveset = IntMap.find var liveness_map in
                 let fb = o#fresh_frame_binder ~var () in
                 Printf.eprintf "Liveset at %d,%s: %s\n%!" var (Var.name_of_binder fb) (show_continuation_point liveset);
                 let o = o#add_frame_binder fb in
                 let (fb, o) = o#binder fb in
                 let fb' = o#fresh_frame_binder () in
                 let o = o#add_frame_binder fb' in
                 let (fb', o) = o#binder fb' in
                 let (b, o) = o#binder b in (* b may be used later on *)
                 let final_frame, o =
                   let (cont, o) = o#pop_cont in
                   let st = o#backup in
                   let (tc, _, o) = (o#with_liveset liveset.after)#tail_computation cont in
                   let liveset' =
                     match tc with
                     (* | Apply (Variable f, _) -> Printf.eprintf "appl: %d\n%!" f; IntMap.find f liveness_map *)
                     | _ -> liveset
                   in
                   let o = o#restore st in
                   let frame =
                     let xsb = o#make_parameters (Some var) (IntSet.to_list liveset'.after) in
                     (fb', ([], xsb, ([], tc)), None, CommonTypes.Location.Unknown)
                   in
                   let cont =
                     Apply (Variable (Var.var_of_binder fb'),
                             o#make_arguments (Some var) (IntSet.to_list liveset'.after))
                   in
                   frame, o#push_cont cont
                 in
                 let answer_frame, o =
                   let (body, o) =
                     let st = o#backup in
                     (* let name = o#get_basename in *)
                     (* (\* let o = (o#reset)#with_basename name in *\) *)
                     let (tc, _, o) = (o#with_liveset liveset.before)#tail_computation tc in
                     let o = o#restore st in
                     let (cont, o) =
                       let ((cont : Ir.tail_computation), o) = o#pop_cont in
                       let (tc, _, o) = o#tail_computation cont in
                       tc, o
                     in
                     ([Let (b, (tyvars, tc))], cont), o
                   in
                   let xsb = o#make_parameters pred (IntSet.to_list liveset.before) in
                   (fb, ([], xsb, body), None, CommonTypes.Location.Unknown), o
                 in
                 let cont =
                   Apply (Variable (Var.var_of_binder fb),
                           o#make_arguments pred (IntSet.to_list liveset.before))
                 in
                 let o = o#push_cont cont in
                 ([answer_frame; final_frame], [], o)
              | Let (b, (tyvars, tc)) :: bs ->
                 let var = Var.var_of_binder b in
                 let liveset = IntMap.find var liveness_map in
                 let fb = o#fresh_frame_binder ~var () in
                 Printf.eprintf "Liveset at %d,%s: %s\n%!" var (Var.name_of_binder fb) (show_continuation_point liveset);
                 let o = o#add_frame_binder fb in
                 let (fb, o) = o#binder fb in
                 let (answer_frames, other, o) =
                   let (_, o) = o#binder b in (* b may be used later on *)
                   generate_frames (o#with_liveset liveset.after) (Some var) bs
                 in
                 let answer_frame, o =
                   let (body, o) =
                     let st = o#backup in
                     (* let name = o#get_basename in *)
                     (* (\* let o = (o#reset)#with_basename name in *\) *)
                     let (tc, _, o) = (o#with_liveset liveset.before)#tail_computation tc in
                     let o = o#restore st in
                     let (cont, o) =
                       let ((cont : Ir.tail_computation), o) = o#pop_cont in
                       let (tc, _, o) = o#tail_computation cont in
                       (* let o = *)
                       (*   match tc with *)
                       (*   | `Apply (`Variable contv, args) -> *)
                       (*      o#register_cont b (contv, args) *)
                       (*   | _ -> assert false *)
                       (* in *)
                       tc, o
                     in
                     ([Let (b, (tyvars, tc))], cont), o
                   in
                   let xsb = o#make_parameters pred (IntSet.to_list liveset.before) in
                   (fb, ([], xsb, body), None, CommonTypes.Location.Unknown), o
                 in
                 let cont =
                   Apply (Variable (Var.var_of_binder fb),
                           o#make_arguments pred (IntSet.to_list liveset.before))
                 in
                 let o = o#push_cont cont in
                 (answer_frame :: answer_frames, other, o)
              (* | Fun (fb, (tyvars, xsb, body), z, loc) :: bs -> *)
              (*    let liveset = IntMap.find (Var.var_of_binder fb) liveness_map in *)
              (*    let st = o#backup in *)
              (*    let o = o#reset in *)
              (*    let (f, o) = *)
              (*      let (xsb, o) = *)
              (*        List.fold_right *)
              (*          (fun b (bs, o) -> *)
              (*            let (b, o) = o#binder b in *)
              (*            (b :: bs, o)) *)
              (*          xsb ([], o) *)
              (*      in *)
              (*      let (z, o) = *)
              (*        match z with *)
              (*        | None -> None, o *)
              (*        | Some z -> *)
              (*           let (z, o) = o#binder z in *)
              (*           (Some z, o) *)
              (*      in *)
              (*      let (body, _, o) = *)
              (*        frame_counter := 0; *)
              (*        (o#with_basename (Var.name_of_binder fb))#computation body *)
              (*      in *)
              (*      let (fb, o) = o#binder fb in *)
              (*      let xsb' = o#make_parameters None (IntSet.to_list liveset.before) in *)
              (*      Fun (fb, (tyvars, xsb' @ xsb, body), z, loc), o *)
              (*    in *)
              (*    let (answer_frames, other, o) = generate_frames (o#restore st) None bs in *)
              (*    (answer_frames, f :: other, o) *)
              (* | Rec defs :: bs -> *)
              (*    let st = o#backup in *)
              (*    let o  = List.fold_left (fun o -> o#fun_binder) o defs in *)
              (*    let (defs, o) = *)
              (*      List.fold_left *)
              (*        (fun (defs, o) (fb, (tyvars, xsb, body), z, loc) -> *)
              (*          (\* Printf.eprintf "Visiting: %s\n%!" (Var.name_of_binder fb); *\) *)
              (*          (\* Printf.eprintf "%s\n%!" (Ir.Show_computation.show body); *\) *)
              (*          frame_counter := 0; *)
              (*          let o = o#reset in *)
              (*          let (xsb, o) = *)
              (*            List.fold_right *)
              (*              (fun b (bs, o) -> *)
              (*                let (b, o) = o#binder b in *)
              (*                (b :: bs, o)) *)
              (*              xsb ([], o) *)
              (*          in *)
              (*          let (z, o) = *)
              (*            match z with *)
              (*            | None -> z, o *)
              (*            | Some z -> *)
              (*               let (z, o) = o#binder z in *)
              (*               (Some z, o) *)
              (*          in *)
              (*          let (body, _, o) = *)
              (*            (o#with_basename (Var.name_of_binder fb))#computation body *)
              (*          in *)
              (*          let def = (fb, (tyvars, xsb, body), z, loc) in *)
              (*          (def :: defs, o)) *)
              (*        ([], o) defs *)
              (*    in *)
              (*    let (answer_frames, other, o) = generate_frames (o#restore st) None bs in *)
              (*    (answer_frames, (Rec (List.rev defs)) :: other, o) *)
              | Fun _ :: _ | Rec _ :: _ -> assert false
              | b :: bs ->
                 let (answer_frames, other, o) = generate_frames o None bs in
                 (answer_frames, b :: other, o)
            in
            let splice other = function
              | [] -> other
              | defs -> other @ [Rec defs]
            in
            let st = o#backup in
            let o = o#push_cont tc in
            let (answer_frames, other, o) = generate_frames o None bs in
            let (cont, o) = o#pop_cont in
            let (cont, dt, o) = o#tail_computation cont in
            (splice other answer_frames, cont), dt, o#restore st

          method! program (bs, tc) =
            let rec gbs o = function
              | Let (b, (tyvars, tc)) :: bs ->
                 let envs = o#backup in
                 let (tc, _, o) = o#tail_computation tc in
                 let (b, o) = o#binder b in
                 let (bs, o) = gbs o bs in
                 let o = o#restore envs in
                 Let (b, (tyvars, tc)) :: bs, o
              | Fun (fb, (tyvars, xsb, body), z, loc) :: bs ->
                 (* let liveset = IntMap.find (Var.var_of_binder fb) liveness_map in *)
                 let st = o#backup in
                 let o = o#reset in
                 let (f, o) =
                   let (xsb, o) =
                     List.fold_right
                       (fun b (bs, o) ->
                         let (b, o) = o#binder b in
                         (b :: bs, o))
                       xsb ([], o)
                   in
                   let (z, o) =
                     match z with
                     | None -> z, o
                     | Some z ->
                        let (z, o) = o#binder z in
                        (Some z, o)
                   in
                   let (body, _, o) =
                     frame_counter := 0;
                     (o#with_basename (Var.name_of_binder fb))#computation body
                   in
                   let (fb, o) = o#binder fb in
                   Fun (fb, (tyvars, xsb, body), z, loc), o
                 in
                 let (bs, o) = gbs (o#restore st) bs in
                 (f :: bs, o)
              | Rec defs :: bs ->
                 let st = o#backup in
                 let o  = List.fold_left (fun o -> o#fun_binder) o defs in
                 let (defs, o) =
                   List.fold_left
                     (fun (defs, o) (fb, (tyvars, xsb, body), z, loc) ->
                       (* Printf.eprintf "Visiting: %s\n%!" (Var.name_of_binder fb); *)
                       (* Printf.eprintf "%s\n%!" (Ir.Show_computation.show body); *)
                       frame_counter := 0;
                       let o = o#reset in
                       let (xsb, o) =
                         List.fold_right
                           (fun b (bs, o) ->
                             let (b, o) = o#binder b in
                             (b :: bs, o))
                           xsb ([], o)
                       in
                       let (z, o) =
                         match z with
                         | None -> z, o
                         | Some z ->
                            let (z, o) = o#binder z in
                            (Some z, o)
                       in
                       let (body, _, o) =
                         (o#with_basename (Var.name_of_binder fb))#computation body
                       in
                       let def = (fb, (tyvars, xsb, body), z, loc) in
                       (def :: defs, o))
                     ([], o) defs
                 in
                 let (bs, o) = gbs (o#restore st) bs in
                 ((Rec (List.rev defs)) :: bs, o)
              | Alien (b,name,lang) :: bs ->
                 let (b, o) = o#binder b in
                 let (bs, o) = gbs o bs in
                 (Alien (b, name, lang) :: bs, o)
              | Module _ :: _ -> assert false
              | [] -> [], o
            in
            let (bs, o) = gbs o bs in
            let (tc, dt, o) = o#tail_computation tc in
            ((bs, tc), dt, o)
        end
      in
      let (prog, _, o) = opt_fragmentise#program prog in
      prog, o#get_frame_binders

  end

  (* The answer frame set is for all practical purposes a global
     constant after the procedure fragmentation pass. *)
  type afrenv = IntSet.t
  let answer_frame_set : afrenv ref = ref IntSet.empty
  let is_answer_frame fb =
    let var = Var.var_of_binder fb in
    IntSet.mem var !answer_frame_set

  let tyenv = ref Env.Int.empty
  let liveness_map = ref IntMap.empty

  (* Frame creation *)
  let new_frame_obj frame_class cont_var args =
    Js.(ENew (EApply (frame_class, cont_var :: args)))

  let new_initial_frame = new_frame_obj (Js.EVar "GenericInitialPureContinuationFrame")

  let new_frame = new_frame_obj (Js.EVar "GenericPureContinuationFrame")

  let extend_cont exn frame =
    let open Js in
    let extend = EApply (EAccess (exn, "continuation.augment"), [frame]) in
    SSeq (SExpr extend, SThrow exn)

  let trycatch make_frame (cont_var, cont_args) m =
    let open Js in
    let exn = Ident.of_string "_exn" in
    let frame =
      make_frame cont_var cont_args
    in
    STry (m, Some (exn,
                   ([], SIf (EApply (EPrim "%instanceof",
                                     [EVar exn; EVar "SaveContinuationError"]),
                             ([], extend_cont (EVar exn) frame),
                             ([], SThrow (EVar exn))))))

  let trampoline cont_var cont_args =
    let open Js in
    let incr = SAssign ("_callcount", EApply (EPrim "%int_add", [EVar "_callcount"; ELit (LInt 1)])) in
    let reset = SAssign ("_callcount", ELit (LInt 0)) in
    let _callcc =
      let identity =
        EFun {
          fkind = `Regular;
          fname = `Anonymous;
          formal_params = ["cont"];
          body = ([], SReturn (EVar "null")) }
      in
      EApply (EVar "Continuation.CWCC", [identity])
    in
    let check =
      let initiate_bounce =
        trycatch
          new_initial_frame
          (cont_var, cont_args)
          ([], SThrow (ENew (EApply (EVar "SaveContinuationError", []))))
      in
      SIf (EApply (EPrim "%ge", [EVar "_callcount"; EVar "_breakat"]),
           ([], SSeq (reset, initiate_bounce)),
           ([], SSkip))
    in
    SSeq (incr, check)

  (* Translation start *)
  let rec generate_program : venv -> Ir.program -> venv * Js.program
    = fun env (bs,tc) ->
      let open Js in
      let open Ir in
      let toplevel body =
        EApply
          (EVar (Ident.of_string "Continuation.establishInitialContinuation"),
           [EFun {
             fname = `Anonymous;
             fkind = `Regular;
             body = body;
             formal_params = [] }])
      in
      let rec gbs env = function
        | Module _ :: bs -> gbs env bs
        | Alien (bnd, raw_name, _lang) :: bs ->
           let (a, _a_name) = safe_name_binder bnd in
           let env' = VEnv.bind env (a, raw_name) in
           gbs env' bs
        | Let (b, (_, tc)) :: bs ->
           let b' = safe_name_binder b in
           let expr =
             match tc with
             | Return v ->
                generate_value env v
             | _ ->
                toplevel (generate_tail_computation env tc)
           in
           let decl =
             DLet {
               bkind = `Const;
               binder = snd b';
               expr
             }
           in
           let (env, decls) = gbs (VEnv.bind env b') bs in
           (env, decl :: decls)
        | Fun (fb, xs, z, loc) :: bs ->
           let fbinding = generate_function env [] (fb, xs, z, loc) in
           let fb = safe_name_binder fb in
           let env', bindings = gbs (VEnv.bind env fb) bs in
           env', (fbinding :: bindings)
        | Rec fundefs :: bs ->
           let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) fundefs in
           let defs = List.map (generate_function env fs) fundefs in
           let env' = List.fold_left VEnv.bind env fs in
           let env'', bindings = gbs env' bs in
           env'', (defs @ bindings)
        | [] -> env, []
      in
      let (env, decls) = gbs env bs in
      let body = generate_tail_computation env tc in
      (env, (decls, SExpr (toplevel body)))


  and generate_computation : venv -> Ir.computation -> venv * Js.program
    = fun env (bs, tc) ->
      (* let open Js in *)
      (* let trycatch m = *)
      (*   let exn = Ident.of_string "_exn" in *)
      (*   STry (m, Some (exn, *)
      (*                  ([], SIf (EApply (EPrim "%instanceof", *)
      (*                                    [EVar exn; EVar "SaveContinuationError"]), *)
      (*                            ([], SBreak), *)
      (*                            ([], SThrow (EVar exn)))))) *)
      (* in *)
      (* let rec capture_continuation env b tc = *)
      (*   let b' = safe_name_binder b in *)
      (*   let binding = *)
      (*     DLet { *)
      (*       bkind = `Const; *)
      (*       binder = snd b'; *)
      (*       expr = *)
      (*         EApply *)
      (*           (EFun { *)
      (*             fname = `Anonymous; *)
      (*             fkind = `Regular; *)
      (*             formal_params = []; *)
      (*             body = ([], trycatch (generate_tail_computation env tc)) }, *)
      (*            []) *)
      (*     } *)
      (*   in *)
      (*   let env' = VEnv.bind env b' in *)
      (*   (\* let (bs, tc) = *\) *)
      (*   (\*   Ir.ReplaceReturnWithApply.program !tyenv assign [`Variable (Var.var_of_binder b)] ([], tc) *\) *)
      (*   (\* in *\) *)
      (*   (\* let prog = *\) *)
      (*   (\*         (\\* HACK, TODO FIX tail_computation -- should return an expression *\\) *\) *)
      (*   (\*   match generate_tail_computation env' tc with *\) *)
      (*   (\*   | [], SReturn (EApply _ as appl) -> ([], SExpr appl) *\) *)
      (*   (\*   | body -> body *\) *)
      (*   (\* in *\) *)
      (*   env', [binding] *)
      (* and gbs env = function *)
      (*   | `Module _ :: bs -> gbs env bs *)
      (*   | `Alien (bnd, raw_name, _lang) :: bs -> *)
      (*      let (a, _a_name) = safe_name_binder bnd in *)
      (*      let env' = VEnv.bind env (a, raw_name) in *)
      (*      gbs env' bs *)
      (*   | `Let (b, (_, tc)) :: bs -> *)
      (*    (\* Printf.printf "Var: %d\n%!" (Var.var_of_binder b); *\) *)
      (*    (\* Printf.printf "Env: %s\n%!" (Show_venv.show env); *\) *)
      (*      let b' = safe_name_binder b in *)
      (*      let env', (bindings, stmt) = gbs (VEnv.bind env b') bs in *)
      (*      let env'', (bindings', stmt') = *)
      (*        match tc with *)
      (*        | `Return v -> *)
      (*           let binding = *)
      (*             DLet { *)
      (*               bkind = `Const; *)
      (*               binder = snd b'; *)
      (*               expr = generate_value env v *)
      (*             } *)
      (*           in *)
      (*           VEnv.bind env' b', ([binding] @ bindings, stmt) *)
      (*        | _ -> *)
      (*           let env', bindings' = capture_continuation env' b tc in *)
      (*           let bindings', stmt' = Js.eliminate_thunks (bindings', stmt) in *)
      (*           env', (bindings' @ bindings, stmt') *)
      (*      in *)
      (*      env'', (bindings', stmt') *)
      (*   | `Fun (fb, xs, z, loc) :: bs -> *)
      (*      let fbinding = generate_function env [] (fb, xs, z, loc) in *)
      (*      let fb = safe_name_binder fb in *)
      (*      let env', (bindings, stmt) = gbs (VEnv.bind env fb) bs in *)
      (*      env', (fbinding :: bindings, stmt) *)
      (*   | `Rec fundefs :: bs -> *)
      (*      let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) fundefs in *)
      (*      let defs = List.map (generate_function env fs) fundefs in *)
      (*      let env' = List.fold_left VEnv.bind env fs in *)
      (*      let env'', (bindings, stmt) = gbs env' bs in *)
      (*      env'', (defs @ bindings, stmt) *)
      (*   | [] -> env, generate_tail_computation env tc *)
      (* in *)
  (* gbs env bs *)
      let (env, bs) = generate_bindings env bs in
      let (bs', stmt) = generate_tail_computation env tc in
      env, (bs @ bs', stmt)

  and generate_bindings env bs =
    let open Ir in
    let rec gbs env = function
      | Module _ :: bs -> gbs env bs
      | Alien (bnd, raw_name, _lang) :: bs ->
         let (a, _a_name) = safe_name_binder bnd in
         let env' = VEnv.bind env (a, raw_name) in
         gbs env' bs
      | Let (_b, (_, _tc)) :: _bs -> assert false
    (*  (\* Printf.printf "Var: %d\n%!" (Var.var_of_binder b); *\) *)
    (* (\* Printf.printf "Env: %s\n%!" (Show_venv.show env); *\) *)
    (* let b' = safe_name_binder b in *)
    (* let env', (bindings, stmt) = gbs (VEnv.bind env b') bs in *)
    (* let env'', (bindings', stmt') = *)
    (*   match tc with *)
    (*   | `Return v -> *)
    (*      let binding = *)
    (*        DLet { *)
    (*          bkind = `Const; *)
    (*          binder = snd b'; *)
    (*          expr = generate_value env v *)
    (*        } *)
    (*      in *)
    (*      VEnv.bind env' b', ([binding] @ bindings, stmt) *)
    (*   | _ -> *)
    (*      let env', bindings' = capture_continuation env' b tc in *)
    (*      let bindings', stmt' = Js.eliminate_thunks (bindings', stmt) in *)
    (*      env', (bindings' @ bindings, stmt') *)
    (* in *)
    (* env'', (bindings', stmt') *)
      | Fun (fb, xs, z, loc) :: bs ->
         let fbinding = generate_function env [] (fb, xs, z, loc) in
         let fb = safe_name_binder fb in
         let env', bindings = gbs (VEnv.bind env fb) bs in
         env', (fbinding :: bindings)
      | Rec fundefs :: bs ->
         let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) fundefs in
         let defs = List.map (generate_function env fs) fundefs in
         let env' = List.fold_left VEnv.bind env fs in
         let env'', bindings = gbs env' bs in
         env'', (defs @ bindings)
      | [] -> env, []
    in
    gbs env bs

  and generate_tail_computation : venv -> Ir.tail_computation -> Js.program
    = fun env tc ->
      let open Js in
      let open Ir in
      (* Printf.eprintf "tc: %s\n%!" (Ir.Show_tail_computation.show tc); *)
      let gv v = generate_value env v in
      let gc c = snd (generate_computation env c) in
      match tc with
      | Return v ->
         [], SReturn (gv v)
      | Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   [], SReturn (Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ())
                | [l; r] when StringOp.is f_name ->
                   [], SReturn (StringOp.gen ~op:f_name ~args:[gv l; gv r] ())
                | [l; r] when Comparison.is f_name ->
                   [], SReturn (Comparison.gen ~op:f_name ~args:[gv l; gv r] ())
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     match f_name, vs with
                     | "deref", [v] ->
                        [], SReturn (EAccess (gv v, "_contents"))
                     | "ref", [v] ->
                        [], SReturn (EObj [("_contents", gv v)])
                     | ":=", [r; v] ->
                        let destructive_update =
                          EApply (EPrim "%assign", [EAccess (gv r, "_contents"); gv v])
                        in
                        [], SSeq (SExpr destructive_update, SReturn (EObj []))
                     | "=Override", [r; v] ->
                        [], SExpr (EApply (EPrim "%assign", [gv r; gv v]))
                     | _ ->
                        let expr =
                          try
                            let args = List.map gv vs in
                            Functions.gen ~op:f_name ~args ()
                          with Not_found -> failwith (Printf.sprintf "Unsupported primitive (tc): %s.\n" f_name)
                        in
                        [], SReturn expr
                   else
                     [], SReturn (EApply (gv (Variable f), List.map gv vs))
              end
           | _ ->
              [], SReturn (EApply (gv f, List.map gv vs))
         end
      | Special special ->
         generate_special env special
      | Case (v, cases, default) ->
         let v = gv v in
         let scrutineeb = Ident.make ~prefix:"_scrutinee" () in
         let bind_scrutinee scrutinee =
           DLet {
             bkind = `Const;
             binder = scrutineeb;
             expr = scrutinee; }
         in
         let open Utility in
         let (decls, prog) =
           let translate_case (xb, c) =
             let (x, x_name) = safe_name_binder xb in
             let value_binding =
               DLet { bkind = `Const;
                      binder = Ident.of_string x_name;
                      expr = EAccess (EVar scrutineeb, "_value"); }
             in
             let (_, (decls, stmt)) = generate_computation (VEnv.bind env (x, x_name)) c in
             value_binding :: decls, stmt
           in
           let cases = StringMap.map translate_case cases in
           let default = opt_map translate_case default in
           [], SCase (EAccess (EVar scrutineeb, "_label"), cases, default)
         in
         decls @ [bind_scrutinee v], prog
      | If (v, c1, c2) ->
         [], SIf (gv v, gc c1, gc c2)
  and generate_special : venv -> Ir.special -> Js.program
    = fun env sp ->
      let open Js in
      let open Ir in
      let gv v = generate_value env v in
      match sp with
      | Wrong _ -> [], SReturn (EApply (EPrim "%error", [ELit (LString "Internal Error: Pattern matching failed")]))
      | DoOperation (name, args, _) ->
         let box = function
           | [v] -> gv v
           | vs -> make_dictionary (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
         in
         let op =
           make_dictionary [ ("_label", strlit name)
                           ; ("_value", make_dictionary [("p", box args)]) ]
         in
         [], SThrow (ENew (EApply (EVar "PerformOperationError", [op])))
      | Handle { Ir.ih_comp = m; Ir.ih_return = return; Ir.ih_cases = cases; Ir.ih_depth = depth } ->
         if depth = Shallow then failwith "Compilation of shallow handlers is not supported.";
         let new_handle_frame value trap =
           ENew (EApply (EVar "GenericHandleFrame", [value; trap]))
         in
         let instantiate_trap, set_abstract_trap =
           let get_cont exn =
             EAccess (exn, "continuation")
           in
           (fun exn value trap ->
             EApply (EAccess (get_cont exn, "instantiateTrapPoint"), [new_handle_frame value trap])),
           (fun exn ->
             EApply (EAccess (get_cont exn, "setAbstractTrapPoint"), []))
         in
         let forward exn value trap =
           ([], SSeq (SExpr (instantiate_trap exn value trap),
                      SSeq (SExpr (set_abstract_trap exn),
                            SThrow exn)))
         in
         let install_trap m trap value =
           let result = Ident.of_string (gensym ~prefix:"_result" ()) in
           let result_decl =
             DLet {
                 bkind = `Let;
                 binder = result;
                 expr = EVar "undefined" }
           in
           let exn_name = Ident.of_string (gensym ~prefix:"_exn" ()) in
           let exn = EVar exn_name in
           ([result_decl], SSeq
                             (STry (([], SAssign (result, m)),
                                    Some (exn_name,
                                          ([], SIf (EApply (EPrim "%instanceof",
                                                            [exn; EVar "PerformOperationError"]),
                                                    ([], SReturn (EApply (trap, [exn]))),
                                                    ([], SIf (EApply (EPrim "%instanceof",
                                                                      [exn; EVar "SaveContinuationError"]),
                                                              forward exn value trap,
                                                              ([], SThrow exn))))))),
                              SReturn (EApply (value, [EVar result]))))
         in
         let value_decl, value =
           let (xb, body) = return in
           let xb' = safe_name_binder xb in
           let valueb = Ident.of_string (gensym ~prefix:"_value" ()) in
           let decl =
             DLet {
                 bkind = `Const;
                 binder = valueb;
                 expr =
                   EFun {
                       fkind = `Regular;
                       fname = `Anonymous;
                       formal_params = [snd xb'];
                       body = snd (generate_computation (VEnv.bind env xb') body)
                     }
               }
           in
           decl, EVar valueb
         in
         let trap_decl, trap =
           let exn = Ident.of_string (gensym ~prefix:"_exn" ()) in
           let trapb = Ident.of_string (gensym ~prefix:"_trap" ()) in
           let trapb' = Ident.of_string (gensym ~prefix:"_trap_self" ()) in
           let op = EAccess (EVar exn, "op") in
           let cases =
             StringMap.fold
               (fun label (xb, rb, body) acc ->
                 let xb', rb' = safe_name_binder xb, safe_name_binder rb in
                 let env' = VEnv.bind (VEnv.bind env xb') rb' in
                 let xdecl =
                   DLet {
                       bkind = `Const;
                       binder = snd (xb');
                       expr = EAccess (EAccess (op, "_value"), "p")
                     }
                 in
                 let rdecl =
                   DLet {
                       bkind = `Const;
                       binder = snd (rb');
                       expr = EApply (EAccess (EVar "_Resumption", "makeDeep"), [EVar exn; value; EVar trapb'])
                     }
                 in
                 let _, (decls,stmt) = generate_computation env' body in
                 let comp = (xdecl :: rdecl :: decls, stmt) in
                 StringMap.add label comp acc)
               cases StringMap.empty
           in
           let trap_body =
             ([],
              SIf (EApply (EPrim "%instanceof", [EVar exn; EVar "PerformOperationError"]),
                   ([], SCase (EAccess (op, "_label"),
                               cases,
                               Some (forward (EVar exn) value (EVar trapb')))),
                   ([], SIf (EApply (EPrim "%instanceof", [EVar exn; EVar "SaveContinuationError"]),
                             (forward (EVar exn) value (EVar trapb')),
                             ([], SThrow (EVar exn))))))
           in
           let decl =
             DLet {
                 bkind = `Const;
                 binder = trapb;
                 expr =
                   EFun {
                       fkind = `Regular;
                       fname = `Named trapb';
                       formal_params = [exn];
                       body = trap_body
                     }
               }
           in
           decl, EVar trapb
         in
         let comp =
           let thunk =
             EFun {
                 fkind = `Regular;
                 fname = `Anonymous;
                 formal_params = [];
                 body = snd (generate_computation env m) }
           in
           EApply (thunk, [])
         in
         let (decls, stmt) = install_trap comp trap value in
         value_decl :: trap_decl :: decls, stmt
         (* (\** **\)
         *  let comp_name = Ident.of_string (gensym ~prefix:"_f" ()) in
         *  let tryhandle result_binder handle_name eff_cases return : Js.statement =
         *   let exn = Ident.of_string (gensym ~prefix:"_exn" ()) in
         *   let op = EAccess (EVar exn, "op") in
         *   let resume_binder, resume_decl =
         *     let binder = gensym ~prefix:"_resume" () in
         *     binder, DLet {
         *                 bkind = `Const;
         *                 binder = binder;
         *                 expr = EApply (EAccess (EVar "_Resumption", "makeDeep"), [EVar exn; EVar handle_name]) }
         *   in
         *   let forward =
         *     let new_handle_frame = ENew (EApply (EVar "GenericHandleFrame", [EVar handle_name])) in
         *     let exn = EVar exn in
         *     let instantiate = SExpr (EApply (EAccess (exn, "continuation.instantiateTrapPoint"), [new_handle_frame])) in
         *     let new_trap = SExpr (EApply (EAccess (exn, "continuation.setAbstractTrapPoint"), [])) in
         *     [], SSeq (instantiate, SSeq (new_trap, SThrow exn))
         *   in
         *   let cases = eff_cases op (EApply (EAccess (EVar "_Resumption", "makeDeep"), [EVar exn; EVar handle_name])) in
         *   let body =
         *     [], SAssign (result_binder, EApply (EVar comp_name, []))
         *   in
         *   STry (body, Some (exn,
         *                     ([], SIf (EApply (EPrim "%instanceof",
         *                                       [EVar exn; EVar "PerformOperationError"]),
         *                               ([], SCase (EAccess (op, "_label"), cases, Some forward)),
         *                               ([], SIf (EApply (EPrim "%instanceof",
         *                                                 [EVar exn; EVar "SaveContinuationError"]),
         *                                         forward,
         *                                         ([], SThrow (EVar exn))))))))
         * in
         * let m_name, m_decl =
         *   let m_name = Ident.of_string (gensym ~prefix:"_comp" ()) in
         *   m_name, DLet {
         *               bkind = `Const;
         *               binder = m_name;
         *               expr =
         *                 EFun {
         *                     fkind = `Regular;
         *                     fname = `Anonymous;
         *                     formal_params = [];
         *                     body = snd (generate_computation env m) }
         *             }
         * in
         * let ident_of_var v = Printf.sprintf "_%d" v in
         * let return_decl, return =
         *   let xb = fst return in
         *   let (liveness : continuation_point) = IntMap.find (Var.var_of_binder xb) !liveness_map in
         *   let live_names = List.map ident_of_var (IntSet.to_list liveness.after) in
         *   let xb' = safe_name_binder xb in
         *   let fname = Ident.of_string (gensym ~prefix:"_value" ()) in
         *   let decl =
         *     DLet {
         *         bkind = `Const;
         *         binder = fname;
         *         expr =
         *           EFun {
         *               fkind = `Regular;
         *               fname = `Anonymous;
         *               formal_params = snd xb' :: live_names;
         *               body = snd (generate_computation (VEnv.bind env xb') (snd return))
         *       } }
         *   in
         *   decl, (fun result -> EApply (EVar fname, result :: (List.map (fun v -> EVar v) live_names)))
         * in
         * let cases op resume =
         *   StringMap.fold
         *     (fun label (xb, rb, body) acc ->
         *       let xb', rb' = safe_name_binder xb, safe_name_binder rb in
         *       let env' = VEnv.bind (VEnv.bind env xb') rb' in
         *       let xdecl =
         *         DLet {
         *           bkind = `Const;
         *           binder = snd (xb');
         *           expr = EAccess (EAccess (op, "_value"), "p")
         *         }
         *       in
         *       let rdecl =
         *         DLet {
         *           bkind = `Const;
         *           binder = snd (rb');
         *           expr = resume
         *         }
         *       in
         *       let _, (decls,stmt) = generate_computation env' body in
         *       let comp = (xdecl :: rdecl :: decls, stmt) in
         *       StringMap.add label comp acc)
         *     cases StringMap.empty
         * in
         * let handle_name = Ident.of_string (gensym ~prefix:"_handle" ()) in
         * let result = gensym ~prefix:"_result" () in
         * let result_decl =
         *     DLet {
         *         bkind = `Let;
         *         binder = result;
         *         expr = EVar "undefined";
         *       }
         *   in
         * let handle_decl =
         *   DFun {
         *       fkind = `Regular;
         *       fname = `Named handle_name;
         *       formal_params = [comp_name];
         *       body = [result_decl], SSeq (tryhandle result handle_name cases return, SReturn (return (EVar result)))
         *     }
         * in
         * [m_decl; return_decl; handle_decl], SReturn (EApply (EVar handle_name, [EVar m_name])) *)
      | _ -> failwith "Unsupported special."

  and generate_value : venv -> Ir.value -> Js.expression
    = fun env ->
      let open Js in
      let open Utility in
      let open Ir in
      let gv v = generate_value env v in
      function
      | Constant c ->
         ELit (
           let open CommonTypes.Constant in
           match c with
           | Int v  -> LInt v
           | Float v  -> LFloat v
           | Bool v   -> LBool v
           | Char v   -> LChar v
           | String v -> LString v)
      | Variable var ->
       (* HACK *)
         let name = VEnv.lookup env var in
         if Arithmetic.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = [x; y];
                  body = [], SReturn (Arithmetic.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if StringOp.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = [x; y];
                  body = [], SReturn (StringOp.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if Comparison.is name then
           let x = Ident.of_string "x" in
           let y = Ident.of_string "y" in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = [x; y];
                  body = [], SReturn (Comparison.gen ~op:name ~args:[EVar x; EVar y] ()) }
         else if Functions.is name then
           let rec replicate x = function
             | 0 -> []
             | n -> x :: (replicate x (n - 1))
           in
           let arity = Functions.arity ~op:name () in
           let formal_params = List.map (fun _ -> Ident.make ()) (replicate () arity) in
           let actual_params = List.map (fun i -> EVar i) formal_params in
           EFun { fname = `Anonymous;
                  fkind = `Regular;
                  formal_params = formal_params;
                  body = [], SReturn (Functions.gen ~op:name ~args:actual_params ()) }
         else
           begin match name with
           | "Nil" -> EPrim "%List.nil"
           |  _ -> EVar name
           end
      | Extend (field_map, rest) ->
         let dict =
           make_dictionary
             (StringMap.fold
                (fun name v dict ->
                  (name, gv v) :: dict)
                field_map [])
         in
         begin
           match rest with
           | None -> dict
           | Some v ->
              EApply (EPrim "%Record.union", [gv v; dict])
         end
      | Project (name, v) ->
         EAccess (gv v, name)
      | Erase (names, v) ->
         EApply (EPrim "%Record.erase",
                 [gv v; make_array (List.map strlit (StringSet.elements names))])
      | Inject (name, v, _t) ->
         make_dictionary [("_label", strlit name); ("_value", gv v)]
    (* erase polymorphism *)
      | TAbs (_, v)
      | TApp (v, _) -> gv v
      | ApplyPure (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | Variable f ->
              let f_name = VEnv.lookup env f in
              begin
                match vs with
                | [l; r] when Arithmetic.is f_name ->
                   Arithmetic.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when StringOp.is f_name ->
                   StringOp.gen ~op:f_name ~args:[gv l; gv r] ()
                | [l; r] when Comparison.is f_name ->
                   Comparison.gen ~op:f_name ~args:[gv l; gv r] ()
                | _ ->
                   if Lib.is_primitive f_name
                     && Lib.primitive_location f_name <> CommonTypes.Location.Server
                   then
                     try
                       Functions.gen ~op:f_name ~args:(List.map gv vs) ()
                     with Not_found -> failwith (Printf.sprintf "Unsupported primitive (val): %s.\n" f_name)
                   else
                     EApply (gv (Variable f), (List.map gv vs))
              end
           | _ ->
              EApply (gv f, List.map gv vs)
         end
      | Closure (f, _, v) ->
         EApply (EPrim "%Closure.apply", [gv (Variable f); gv v])
      | Coerce (v, _) ->
         gv v
      | _ -> failwith "Unsupported value."

  and generate_function : venv -> (Var.var * string) list -> Ir.fun_def -> Js.decl =
    fun env fs (fb, (_, xsb, body), zb, location) ->
    let open Js in
    let open Ir in
      (* let new_frame_obj frame_class cont_var args = *)
      (*   ENew (EApply (frame_class, cont_var :: args)) *)
      (* in *)
      (* let new_initial_frame = new_frame_obj (EVar "GenericInitialContinuationFrame") in *)
      (* let new_frame = new_frame_obj (EVar "GenericContinuationFrame") in *)
      (* let extend_cont exn frame = *)
      (*   let extend = EApply (EAccess (exn, "extend"), [frame]) in *)
      (*   SSeq (SExpr extend, SExpr (EThrow exn)) *)
      (* in *)
      (* let trycatch make_frame (cont_var, cont_args) m = *)
      (*   let exn = Ident.of_string "_exn" in *)
      (*   let frame = *)
      (*     make_frame cont_var cont_args *)
      (*   in *)
      (*   STry (m, Some (exn, *)
      (*                  ([], SIf (EApply (EPrim "%instanceof", *)
      (*                                    [EVar exn; EVar "SaveContinuationError"]), *)
      (*                            ([], extend_cont (EVar exn) frame), *)
      (*                            ([], SExpr (EThrow (EVar exn))))))) *)
      (* in *)
      let capture_continuation env (bs,tc) =
        let rec gbs env = function
          | [] -> env, []
          | [(Let (b, (_, tc')))] ->
             let b' = safe_name_binder b in
             let env' = VEnv.bind env b' in
             let binding =
               match tc' with
               | Return v ->
                  DLet {
                    bkind = `Const;
                    binder = snd b';
                    expr = generate_value env v }
               | tc' ->
                  let cont_var, cont_args =
                    match tc with
                    | Apply (cont_var, args) ->
                       generate_value env cont_var, List.(map (generate_value env) (tl args))
                    | _ -> assert false
                  in
                  DLet {
                    bkind = `Const;
                    binder = snd b';
                    expr =
                      EApply
                        (EFun {
                          fname = `Anonymous;
                          fkind = `Regular;
                          formal_params = [];
                          body = ([], trycatch new_frame (cont_var, cont_args) (generate_tail_computation env tc')) },
                         []) }
             in
             env', [binding]
          | Rec fundefs :: bs ->
             let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) fundefs in
             let defs = List.map (generate_function env fs) fundefs in
             let env' = List.fold_left VEnv.bind env fs in
             let env'', bindings = gbs env' bs in
             env'', (defs @ bindings)
          | _ -> assert false
        in
        let (env, decls) = gbs env bs in
        let (decls', stmt) = generate_tail_computation env tc in
        let prog = Js.eliminate_thunks (decls @ decls', stmt) in
        env, prog
      in
      Printf.eprintf "Generating fb (%d, %s); answer frame? %s\n%!" (Var.var_of_binder fb) (Var.name_of_binder fb) (if is_answer_frame fb then "True" else "False");
      let open Js in
      let (_f, f_name) = safe_name_binder fb in
      assert (f_name <> "");
      (* prerr_endline ("f_name: "^f_name); *)
      (* optionally add an additional closure environment argument *)
      let xsb =
        match zb with
        | None -> xsb
        | Some zb -> zb :: xsb
      in
      let bs = List.map safe_name_binder xsb in
      let _xs, xs_names = List.split bs in
      let body_env = List.fold_left VEnv.bind env (fs @ bs) in
      let body =
        let open CommonTypes.Location in
        match location with
        | Client | Unknown when is_answer_frame fb ->
           (* Printf.eprintf "Generating body: %s\n%!" (Ir.Show_computation.show body); *)
           snd (capture_continuation body_env body)
        | Client | Unknown ->
           begin match body with
           | (bs, Apply (cont_var, args)) ->
              let env', bs = generate_bindings body_env bs in
              let cont_var, cont_args =
                generate_value env' cont_var, List.map (generate_value env') args
              in
              let trampoline = trampoline cont_var cont_args in
              (bs, SSeq (trampoline, SReturn (EApply (cont_var, cont_args))))
           | (bs, Return v) ->
              let env', bs = generate_bindings body_env bs in
              (bs, SReturn (generate_value env' v))
           | _ ->
              let cont_var = EVar f_name in
              let cont_args = List.map (fun v -> EVar v) xs_names in
              (* Printf.eprintf "Generating body: %s\n%!" (Ir.Show_computation.show body); *)
              let _, (decls, stmt) = generate_computation body_env body in
              (decls, SSeq (trampoline cont_var cont_args, stmt))
           end
        | _ -> failwith "Only client side calls are supported."
      in
      Printf.eprintf "Done (%d, %s)\n%!" (Var.var_of_binder fb) (Var.name_of_binder fb);
      DFun {
        fname = `Named (Ident.of_string f_name);
        fkind = `Regular;
        formal_params = (List.map Ident.of_string xs_names);
        body; }

  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let (_nenv, venv, tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      let tyenv', nenv = IrTraversals.NameMap.(compute Lib.primitive_name tenv u.program) in
      Printf.eprintf "nenv: %s\n%!" (IrTraversals.NameMap.show_name_map nenv);
      Printf.eprintf "Before fragmentation: %s\n%!" (Ir.show_program u.program);
      (* let lm = Ir.ProcedureFragmentation.liveness tenv u.program in *)
      (* Printf.eprintf "%s\n%!" (string_of_liveness_map venv lm); *)
      let prog =
        let prog = IrTraversals.EtaTailDos.program tenv u.program in
        liveness_map := ProcedureFragmentation.liveness tenv prog;
        let prog, answer_frames = ProcedureFragmentation.opt_fragmentise tenv !liveness_map prog in
        answer_frame_set := answer_frames;
        tyenv := tyenv'; prog
      in
      Printf.eprintf "Fragmented: %s\n%!" (Ir.show_program prog);
      let _, prog = generate_program venv prog in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["base.js"; "array.js"; "performance.js"; "stack.js"] in
      { u with program = prog; includes = u.includes @ dependencies }
end

(* Compiler selection *)
module Compiler =
  (val
      (match Settings.get_value Basicsettings.Js.backend with
      | "cps" ->
         (module CPS : JS_COMPILER)
      | "geniter" ->
         (module TrampolinedGenIter : JS_COMPILER)
      | "cek" ->
         (module CEK : JS_COMPILER)
      | "stackinspection" ->
         (module StackInspection : JS_COMPILER)
      (* TODO: better error handling *)
      | _ -> failwith "Unrecognised JS backend.") : JS_COMPILER)
