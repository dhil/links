type comp_unit = Js.program Js.comp_unit

module type STATE = sig
  type ('s, 'a) t

  val pure : 'a -> ('s, 'a) t
  val (>>=) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t

  val run : init:'s -> ('s, 'a) t -> ('a * 's)
  val eval : init:'s -> ('s, 'a) t -> 'a
  val exec : init:'s -> ('s, 'a) t -> 's
  val get : ('s, 's) t
  val put : 's -> ('s, unit) t
end

module State : STATE = struct
  type ('s, 'a) t = 's -> ('a * 's)

  let run ~init m = m init
  let eval ~init m = fst (run ~init m)
  let exec ~init m = snd (run ~init m)

  let get = fun st -> (st, st)
  let put st = fun _ -> ((), st)

  let pure x = fun st -> (x, st)
  let (>>=) m k =
    fun st ->
      let (x,st') = run ~init:st m in
      run ~init:st' (k x)
end

module type CODEGEN = sig
  type 'a t
  type js
  type ident = Js.Ident.t

  val pure : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val emit : js -> unit -> unit t
  val persist : 'a t -> 'a t

  val gen : comp_unit -> unit t -> unit

  (* val mapM : ('a -> 'b t) -> 'a list -> 'b list t *)
  val sequence : js list -> js

  module Aux: sig
    val newline : js
    val comment : string -> js
    val strict_mode : js
  end

  module Decl: sig
    val let_binding : [`Const | `Let | `Var] -> ident -> js -> js
    val fun_binding : ident -> ident list -> (js list * js) -> js
  end

  module Expr: sig
    val var : ident -> js
    val apply : js -> js list -> js
    val anon_fun : ident list -> (js list * js) -> js
  end

  module Stmt: sig
    val return : js -> js
    val expr : js -> js
    val ifthenelse : js -> (js list * js) -> (js list * js) -> js
  end

  module Prim: sig
    val name : string -> js
    val apply_binop : string -> js * js -> js
  end
end

module CodeGen : CODEGEN = struct
  type js = PP.doc
  type 'a t = (comp_unit * js, 'a) State.t
  type ident = Js.Ident.t

  module S = State
  let pure x = S.pure x
  let (>>=) m k = S.(m >>= k)

  let emit js' () =
    let open PP in
    S.get >>= fun (cu, js) ->
    S.put (cu, js ^^ js')

  let persist _ = assert false

  let gen cu code =
    let (_, st) = State.run ~init:(cu, PP.empty) code in
    Printf.printf "%s\n%!" (PP.pretty 144 (snd st));
    ()

  let sequence : (js -> js) -> js list -> js
    = fun sep xs ->
      List.fold_left
        (fun acc x -> PP.(acc ^| (sep x)))
        PP.empty xs

  module Aux = struct
    let newline = PP.(breakWith nl)

    let comment txt =
      let open PP in
      (text "/*") ^+^ (text txt) ^+^ (text "*/")

    let strict_mode = PP.(text "'use strict';")
  end

  let layout_program : (js list * js) -> js
    = fun (decls, stmt) ->
      let open PP in
      let decls' = sequence (fun d -> d) decls in
      (group (nest 2 (decls' ^| stmt)))

  module Decl = struct
    let semi : js -> js
      = fun stmt ->
        PP.(stmt ^^ (text ";"))

    let let_binding kind binder expr =
      let kind =
        match kind with
        | `Const -> "const"
        | `Let   -> "let"
        | `Var   -> "var"
        | _      -> assert false
      in
      let open PP in
      semi @@ (text kind) ^+^ (text binder) ^+^ (text "=") ^+^ expr

    let fun_binding binder params prog =
      let open PP in
      (text "function") ^+^ (text binder) ^^ (formal_list params)
      ^+^ (text "{" ^| (group (nest 2 ((layout_program prog)))) ^^ (text "\b\b}")) (* HACK *)
  end

  module Expr = struct
    let var ident = PP.text ident
    let anon_fun params prog =
      let open PP in
      group
        (nest 2
           (group (nest 2 (text "function" ^^ (formal_list params) ^+^ (text "{"))))
         ^| (group (nest 2 (layout_program prog))))
          ^| (text "\b\b}")
    let apply f args = PP.(f ^^ (arglist args))
  end

  module Stmt = struct
    let return e = Decl.semi (PP.(text "return" ^+^ (parens e)))
    let expr e   = Decl.semi e
    let ifthenelse cond tt ff =
      let open PP in
      group (nest 2 (
        (group (nest 2 (text "if" ^| (parens cond) ^+^ (text "{")))
         ^| group (nest 2 (layout_program tt))
           ^| group (nest 2 (text "\b\b} else {"))
             ^| group (nest 2 (layout_program ff))
                 ^| (text "\b\b}"))))
  end

  module Prim = struct
    let name = PP.text

    let apply_binop op (x,y) =
      let open PP in
      x ^+^ (text op) ^+^ y
  end

  (* let (<+>) l r = PP.(l ^^ r) *)

  (* let sequence : js list t -> js t *)
  (*   = fun xs -> *)
  (*     xs >>= fun xs -> *)
  (*     pure @@ List.fold_left (<+>) PP.empty (List.map (fun x -> (Decl.semi x) <+> Aux.newline)  xs) *)

  (* let mapM : ('a -> 'b t) -> 'a list -> 'b list t *)
  (*   = fun f code -> *)
  (*     List.fold_left *)
  (*       (fun acc code -> *)
  (*         (f code) >>= fun x -> *)
(*         acc >>= fun acc -> pure @@ x :: acc) (pure []) code *)

  let sequence = function
    | [] -> PP.empty
    | [x] -> x
    | x :: xs ->
       PP.(x ^^ (sequence (fun x -> Aux.newline ^^ x)) xs)
end

let is_primitive : string -> bool
  = fun p -> String.length p > 0 && p.[0] = '%'

let rec program' : Js.program -> (CodeGen.js list * CodeGen.js)
  = fun (decls, stmt) ->
    let open CodeGen in
    let decls = List.map (fun decl -> declaration decl) decls in
    decls, statement stmt

and declaration : Js.decl -> CodeGen.js
  = function
    | DLet { bkind; binder; expr } ->
       CodeGen.Decl.let_binding bkind binder (expression expr)
    | DFun { fkind = `Named binder; formal_params; body } ->
       CodeGen.Decl.fun_binding binder formal_params (program' body)
    | _ -> assert false

and expression : Js.expression -> CodeGen.js = function
  | EVar name -> CodeGen.Expr.var name
  | EFun { fkind = `Anonymous; formal_params; body } ->
     let open CodeGen in
     Expr.anon_fun formal_params (program' body)
  | EApply (EPrim p, args) ->
     let open CodeGen.Prim in
     let open CodeGen.Expr in
     let pop2 : CodeGen.js list -> CodeGen.js * CodeGen.js
       = fun args -> List.nth args 0, List.nth args 1
     in
     let args' = List.map expression args in
     begin match p with
     | "%eq" -> apply_binop "===" (pop2 args')
     | "%neq" -> apply_binop "!==" (pop2 args')
     | "%lt" -> apply_binop "<" (pop2 args')
     | "%gt" -> apply_binop ">" (pop2 args')
     | "%le" -> apply_binop "<=" (pop2 args')
     | "%ge" -> apply_binop ">=" (pop2 args')

     | "%int_add" | "%float_add" -> apply_binop "+" (pop2 args')
     | "%int_sub" | "%float_sub" -> apply_binop "-" (pop2 args')

     | p when String.length p > 0 ->
        let ident = Printf.sprintf "_%s" (String.sub p 1 (String.length p - 1)) in
        apply (name ident) args'
     | _ -> assert false
     end
  | EApply (f, args) ->
     let open CodeGen in
     Expr.apply (expression f) (List.map expression args)
  | EObj _ -> assert false
  | ELit l -> literal l
  | EFun _ -> assert false
  | EAccess _ -> assert false
  | EPrim p -> primitive p

and statement : Js.statement -> CodeGen.js = function
  | SReturn e -> CodeGen.Stmt.return (expression e)
  | SSeq _ -> assert false
  | SExpr e -> CodeGen.Stmt.expr (expression e)
  | SDie s -> assert false
  | SProg _ -> assert false
  | SCase _ -> assert false
  | SIf (cond,tt,ff) ->
     CodeGen.Stmt.ifthenelse (expression cond) (program' tt) (program' ff)
  | _ -> assert false

and statement_toplevel : Js.statement -> CodeGen.js = function
  | SReturn e -> statement (SExpr e)
  | stmt -> statement stmt

and primitive : string -> CodeGen.js
  = fun p ->
  let open CodeGen.Prim in
  let ident = Printf.sprintf "_%s" (String.sub p 1 (String.length p - 1)) in
  CodeGen.Prim.name ident

and literal : Js.literal -> CodeGen.js = let open CodeGen.Prim in function
  | LBool true -> name "true"
  | LBool false -> name "false"
  | LInt n -> name (string_of_int n)
  | LFloat f -> name (string_of_float f)
  | LString s -> name (Printf.sprintf "\"%s\"" s)
  | LChar c   -> name (Printf.sprintf "'%c'" c)


let emit : program:comp_unit -> unit -> unit
  = fun ~program () ->
    let open CodeGen in
    let (decls, stmt) = program' program.program in
    let prog =
      pure () >>=
        emit (Aux.comment "Program generated...") >>=
        emit (Aux.newline) >>=
        emit (Aux.strict_mode) >>=
        emit (Aux.newline) >>=
        emit (Aux.newline) >>=
        emit (Aux.comment "[Begin] Bindings") >>=
        emit (Aux.newline) >>=
        emit (sequence decls) >>=
        emit (Aux.newline) >>=
        emit (Aux.comment "[End] Bindings") >>=
        emit (Aux.newline) >>=
        emit (Aux.newline) >>=
        emit stmt
    in
    gen program prog
