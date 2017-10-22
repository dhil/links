type comp_unit = Ir.program Js.comp_unit
type prog_unit = Js.program Js.comp_unit

(* Environment *)
module VEnv = Env.Int
type venv = string VEnv.t

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
    | "/" -> "%int_div",2  | "/." -> "%float_div",2
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
    | "debug" -> "%IO.debug",1
    | "not"   -> "%not",1
    | "negatef" | "negate" -> "%negate",1
    | "Cons"  -> "%List.cons",2 | "tl" -> "%List.tail",1  | "hd" -> "%List.head",1
    | "length" -> "%List.length", 1
    | "Concat" -> "%List.concat",2
    | "error"  -> "%IO.error",1
    | "print"  -> "%IO.print",1
    | "now"    -> "%now",0
    | "intToString" | "floatToString" -> "%String.ofNumber",1
    | "floatToInt" -> "%Number.toInt",1 | "intToFloat" -> "%Number.toFloat",1
    | _ -> raise Not_found
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
    make_dictionary @@ List.mapi (fun i e -> (string_of_int i, e)) elements

let strlit : Ir.name -> Js.expression
  = fun s -> Js.(ELit (LString s))

(* strip any top level polymorphism from an expression *)
let rec strip_poly = function
  | `TAbs (_, e)
  | `TApp (e, _) -> strip_poly e
  | e -> e

(** Generate a JavaScript name from a binder, wordifying symbolic names *)
let safe_name_binder (x, info) =
  let name = Js.name_binder (x, info) in
  if (name = "") then
    prerr_endline (Ir.Show_binder.show (x, info))
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
      let gv v = generate_value env v in
      function
      | `Constant c ->
         ELit (
           match c with
           | `Int v  -> LInt v
           | `Float v  -> LFloat v
           | `Bool v   -> LBool v
           | `Char v   -> LChar v
           | `String v -> LString v)
      | `Variable var ->
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
      | `Extend (field_map, rest) ->
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
      | `Project (name, v) ->
         EAccess (gv v, name)
      | `Erase (names, v) ->
         EApply (EPrim "%Record.erase",
                 [gv v; make_array (List.map strlit (StringSet.elements names))])
      | `Inject (name, v, _t) ->
         make_dictionary [("_label", strlit name); ("_value", gv v)]
    (* erase polymorphism *)
      | `TAbs (_, v)
      | `TApp (v, _) -> gv v
      | `ApplyPure (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | `Variable f ->
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
                     && Lib.primitive_location f_name <> `Server
                   then
                     try
                       Functions.gen ~op:f_name ~args:(List.map gv vs) ()
                     with Not_found -> failwith (Printf.sprintf "Unsupported primitive (val): %s.\n" f_name)
                   else
                     EApply (gv (`Variable f), (List.map gv vs))
              end
           | _ ->
              EApply (gv f, List.map gv vs)
         end
      | `Closure (f, v) ->
         EApply (EPrim "%Closure.apply", [gv (`Variable f); gv v])
      | `Coerce (v, _) ->
         gv v
      | _ -> failwith "Unsupported value."

  and generate_tail_computation : venv -> Ir.tail_computation -> continuation -> Js.program
    = fun env tc kappa ->
      let open Js in
      let gv v = generate_value env v in
      let gc c kappa = snd (generate_computation env c kappa) in
      match tc with
      | `Return v ->
         [], SReturn (K.apply kappa (gv v))
      | `Apply (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | `Variable f ->
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
                     && Lib.primitive_location f_name <> `Server
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
                            let args = (List.map gv vs) @ [k] in
                            Functions.gen ~op:f_name ~args ()
                          with Not_found -> failwith (Printf.sprintf "Unsupported primitive (tc): %s.\n" f_name)
                        in
                        [], SReturn expr
                   else
                     let k = K.reify kappa in
                     [], SReturn (EApply (gv (`Variable f), (List.map gv vs) @ [k]))
              end
           | _ ->
              let k = K.reify kappa in
              [], SReturn (EApply (gv f, (List.map gv vs) @ [k]))
         end
      | `Special special ->
         generate_special env special kappa
      | `Case (v, cases, default) ->
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
      | `If (v, c1, c2) ->
         K.bind kappa
           (fun kappa ->
             [], SIf (gv v, gc c1 kappa, gc c2 kappa))

  and generate_special : venv -> Ir.special -> continuation -> Js.program
    = fun env sp kappa ->
      let open Js in
      let gv v = generate_value env v in
      match sp with
      | `Wrong _ -> [], SReturn (EApply (EPrim "%error", [ELit (LString "Internal Error: Pattern matching failed")]))
      | `DoOperation (name, args, _) ->
         let box vs =
           make_dictionary (List.mapi (fun i v -> (string_of_int @@ i + 1, gv v)) vs)
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
      | `Handle { Ir.ih_comp = comp; Ir.ih_clauses = clauses; _ } ->
         let open Utility in
         (* Generate body *)
         let gb env binder body kappas =
           let env' = VEnv.bind env (safe_name_binder binder) in
           snd (generate_computation env' body kappas)
         in
         let (return_clause, operation_clauses) = StringMap.pop "Return" clauses in
         let return =
           let (_, xb, body) = return_clause in
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
           let gc env (ct, xb, body) kappas _z =
             let x_name = snd @@ safe_name_binder xb in
             let env', r_name =
               match ct with
               | `ResumptionBinder rb ->
                  let rb' = safe_name_binder rb in
                  VEnv.bind env rb', snd rb'
               | _ ->
                  let dummy_binder = Var.fresh_binder (Var.make_local_info (`Not_typed, "_dummy_resume")) in
                  let dummy = safe_name_binder dummy_binder in
                  VEnv.bind env dummy, snd dummy
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
      let rec gbs : venv -> continuation -> Ir.binding list -> venv * Js.program =
        fun env kappa ->
          function
          | `Let (b, (_, `Return v)) :: bs ->
             let (x, x_name) = safe_name_binder b in
             let env', (rest, prog) = gbs (VEnv.bind env (x, x_name)) kappa bs in
             let x_binding =
               DLet {
                 bkind = `Const;
                 binder = Ident.of_string x_name;
                 expr = generate_value env v; }
             in
             (env', (x_binding :: rest, prog))
          | `Let (b, (_, tc)) :: bs ->
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
          | `Fun ((fb, _, _zs, _location) as def) :: bs ->
             let (f, f_name) = safe_name_binder fb in
             let def_header = generate_function env [] def in
             let env', (rest, prog) = gbs (VEnv.bind env (f, f_name)) kappa bs in
             (env', (def_header :: rest, prog))
          | `Rec defs :: bs ->
             let fs = List.map (fun (fb, _, _, _) -> safe_name_binder fb) defs in
             let env', (rest, prog) = gbs (List.fold_left VEnv.bind env fs) kappa bs in
             let defs = List.map (generate_function env fs) defs in
             (env', (defs @ rest, prog))
          | `Module _ :: bs
          | `Alien _ :: bs -> gbs env kappa bs
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
      let _,(decls,_) = generate_computation env (bs, `Special (`Wrong `Not_typed)) kappa in
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
        match location with
        | `Client | `Unknown ->
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
      let (_nenv, venv, _tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      (* Printf.printf "nenv:\n%s\n%!" (string_of_nenv u.envs.nenv); *)
      (* Printf.printf "venv:\n%s\n%!" (string_of_venv venv); *)
      let (_,prog) = generate_program venv u.program K.toplevel in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["base.js"; "cps.js"] in
      { u with program = prog; includes = dependencies @ u.includes }
end

(** Generator / Iterator compiler **)
module GenIter = struct

  let rec generate_program : venv -> Ir.program -> venv * Js.program
    = fun venv prog -> assert false
  and generate_computation : venv -> Ir.computation -> venv * Js.program
    = fun venv comp -> assert false
  and generate_tail_computation : venv -> Ir.tail_computation -> Js.program
    = fun venv tc -> assert false
  and generate_special : venv -> Ir.special -> Js.program
    = fun venv special -> assert false
  and generate_value : venv -> Ir.value -> Js.expression
    = fun env ->
      let open Js in
      let open Utility in
      let gv v = generate_value env v in
      function
      | `Constant c ->
         ELit (
           match c with
           | `Int v  -> LInt v
           | `Float v  -> LFloat v
           | `Bool v   -> LBool v
           | `Char v   -> LChar v
           | `String v -> LString v)
      | `Variable var ->
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
      | `Extend (field_map, rest) ->
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
      | `Project (name, v) ->
         EAccess (gv v, name)
      | `Erase (names, v) ->
         EApply (EPrim "%Record.erase",
                 [gv v; make_array (List.map strlit (StringSet.elements names))])
      | `Inject (name, v, _t) ->
         make_dictionary [("_label", strlit name); ("_value", gv v)]
    (* erase polymorphism *)
      | `TAbs (_, v)
      | `TApp (v, _) -> gv v
      | `ApplyPure (f, vs) ->
         let f = strip_poly f in
         begin
           match f with
           | `Variable f ->
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
                     && Lib.primitive_location f_name <> `Server
                   then
                     try
                       Functions.gen ~op:f_name ~args:(List.map gv vs) ()
                     with Not_found -> failwith (Printf.sprintf "Unsupported primitive (val): %s.\n" f_name)
                   else
                     EApply (gv (`Variable f), (List.map gv vs))
              end
           | _ ->
              EApply (gv f, List.map gv vs)
         end
      | `Closure (f, v) ->
         EApply (EPrim "%Closure.apply", [gv (`Variable f); gv v]) (* The closure needs to be generator? *)
      | `Coerce (v, _) ->
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
        match location with
        | `Client | `Unknown ->
           snd (generate_computation body_env body)
        | _ -> failwith "Only client side calls are supported."
      in
      DFun {
        fname = `Named (Ident.of_string f_name);
        fkind = `Generator;
        formal_params = (List.map Ident.of_string xs_names);
        body; }


  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let (_nenv, venv, _tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      let (_,prog) = generate_program venv u.program in
      let dependencies = List.map (fun f -> Filename.concat (Settings.get_value Basicsettings.Js.lib_dir) f) ["base.js"; "geniter.js"] in
      { u with program = prog; includes = dependencies @ u.includes }
end

(* Compiler selection *)
module Compiler =
  (val
      (match Settings.get_value Basicsettings.Js.backend with
      | "cps" ->
         (module CPS : JS_COMPILER)
      | "geniter" ->
         (module GenIter : JS_COMPILER)
      (* TODO: better error handling *)
      | _ -> failwith "Unrecognised JS backend.") : JS_COMPILER)
