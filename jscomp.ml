type comp_unit = Ir.program Js.comp_unit
type prog_unit = Js.program Js.comp_unit

(* Environment *)
module VEnv = Env.Int
type venv = string VEnv.t

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
  val builtin_ops : string list
  val gen : (string -> 'a) -> string -> 'a
end

module Primitives(P : PRIM_DESC) = struct
  let is : string -> bool
    = fun x -> List.mem x P.builtin_ops

  let gen : (Js.expression * string * Js.expression) -> Js.expression
    = fun (l, op, r) ->
      let make op =
        let open Js in
        EApply (EPrim (PFun op), [l; r])
      in
      try
        P.gen make op
      with Not_found -> failwith (Printf.sprintf "Unknown primitive operation %s" op)
end

module Prim_Arithmetic : PRIM_DESC = struct
  let builtin_ops =
    [ "+"; "+."; "-"; "-."; "*"; "*."; "/"; "^"; "^."; "/."; "mod"]

  let gen make = function
    | "+" -> make "%iadd"  | "+." -> make "%fadd"
    | "-"  -> make "%isub" | "-." -> make "%fsub"
    | "/" -> make "%idiv"  | "/." -> make "%fdiv"
    | "^" -> make "%ipow"  | "^." -> make "%fpow"
    | "mod" -> make "%mod"
    | _ -> raise Not_found
end

module Prim_String : PRIM_DESC = struct
  let builtin_ops = ["^^"]
  let gen make = function
    | "^^" -> make "%strconcat"
    | _ -> raise Not_found
end

module Prim_Comparison : PRIM_DESC = struct
  let builtin_ops = ["=="; "<>"; "<"; ">"; "<="; ">="]
  let gen make = function
    | "==" -> make "%eq" | "<>" -> make "%neq"
    | "<"  -> make "%lt" | ">"  -> make "%gt"
    | "<=" -> make "%le" | ">=" -> make "%ge"
    | _ -> raise Not_found
end

module StringOp = Primitives(Prim_String)
module Arithmetic = Primitives(Prim_Arithmetic)
module Comparison = Primitives(Prim_Comparison)

(* Js compilers *)
module type JS_COMPILER = sig
  val compile : comp_unit -> prog_unit
end

module CPS = struct
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
    val pop : t -> (Js.statement -> Js.statement) * t * t

  (* Turns code into a continuation. *)
    val reflect : Js.expression -> t
  (* Turns a continuation into code. *)
    val reify   : t -> Js.expression

  (* Continuation name binding. *)
    val bind : t -> (t -> Js.statement) -> Js.statement

  (* Continuation application generation. The optional strategy
     parameter decides whether the application should be yielding or
     direct. *)
    val apply : t -> Js.expression -> Js.expression
    end = struct
  (* We can think of this particular continuation structure as a
     nonempty stack with an even number of elements. *)
      type t = Cons of Js.expression * t
             | Reflect of Js.expression
             | Identity

  (* Auxiliary functions for manipulating the continuation stack *)
      let nil = Js.(EPrim (PVar "lsNil"))
      let cons x xs = Js.(EApply (EPrim (PFun "%cons"), [x; xs]))
      let head xs = Js.(EApply (EPrim (PFun "%head"), [xs]))
      let tail xs = Js.(EApply (EPrim (PFun "%tail"), [xs]))
      let toplevel = Js.(Cons (EPrim (PVar "%idk"), Cons (EPrim (PVar "%efferr"), Reflect nil)))

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

      let bind kappas body =
        let open Js in
        let gen_bind : Js.Ident.t -> Js.expression -> Js.statement -> Js.statement
          = fun b e c ->
          SSeq (SBind (`Const, b, e), c)
        in
        let rec bind bs ks =
          function
          | Identity ->
             let k = Js.Ident.make ~prefix:"_kappa" () in
             (fun code -> bs (gen_bind k (reify Identity) code)), ks, EVar k
          | Reflect ((EVar _) as v) ->
             bs, ks, v
          | Reflect v ->
             let k = Js.Ident.make ~prefix:"_kappa" () in
             (fun code -> bs (gen_bind k v code)), ks, EVar k
          | Cons ((EVar _) as v, kappas) ->
             bind bs (fun kappas -> Cons (v, kappas)) kappas
          | Cons (v, kappas) ->
             let k = Js.Ident.make ~prefix:"_kappa" () in
             bind
               (fun code -> bs (gen_bind k v code))
               (fun kappas -> Cons (EVar k, kappas)) kappas
        in
        let bs, ks, seed = bind (fun code -> code) (fun kappas -> kappas) kappas in
        bs (body (ks (reflect seed)))

      let apply k arg =
        Js.(EApply (EPrim (PFun "%applyCont"), [reify k; arg]))

      let rec pop = function
        | Cons (kappa, kappas) ->
           (fun code -> code), (reflect kappa), kappas
        | Reflect ks ->
           let open Js in
           let __k = Ident.make ~prefix:"__k" () in
           let __ks = Ident.make ~prefix:"__ks" () in
           (fun code ->
             SSeq (SBind (`Const, __k, head ks),
                  (SSeq (SBind (`Const, __ks, tail ks), code)))),
           (reflect (EVar __k)), reflect (EVar __ks)
        | Identity -> pop toplevel
    end
  type continuation = K.t

  let rec generate_value : venv -> Ir.value -> Js.expression
    = fun env -> function
    | _ -> failwith "Not yet implemented"

  and tail_computation : venv -> continuation -> Ir.tail_computation -> Js.statement
    = fun env kappa -> function
    | _ -> failwith "Not yet implemented"

  and special : venv -> continuation -> Ir.special -> Js.statement
    = fun env kappa -> function
    | _ -> failwith "Not yet implemented"

  and computation : venv -> continuation -> Ir.computation -> Js.statement
    = fun env kappa comp -> failwith "Not yet implemented"

  let compile : comp_unit -> prog_unit
    = fun u ->
      let open Js in
      let (_nenv, venv, _tenv) = initialise_envs (u.envs.nenv, u.envs.tenv) in
      let prog = computation venv K.toplevel u.program in
      { u with program = [prog] }
end


module Compiler =
  (val
      (match Settings.get_value Basicsettings.Js.backend with
      | "cps" ->
         (module CPS : JS_COMPILER)
      (* TODO: better error handling *)
      | _ -> failwith "Unrecognised JS backend.") : JS_COMPILER)
