type comp_unit = Js.program Js.comp_unit

module PP = Prettier

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
  type program = js list * js
  type label = string
  type fnkind = [`Regular | `Generator]

  val pure : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val emit : js -> unit -> unit t
  val emit_runtime : unit -> unit t
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
    val fun_binding : fnkind -> ident -> ident list -> program -> js
  end

  module Expr: sig
    val var : ident -> js
    val apply : js -> js list -> js
    val anon_fun : fnkind -> ident list -> (js list * js) -> js
    val access : js -> label -> js
    val object' : (label * js) list -> js
    val array : js array -> js
    val yield : [`Regular | `Star] -> js -> js
    val throw : js -> js
    val new' : js -> js
  end

  module Stmt: sig
    val return : js -> js
    val expr : js -> js
    val ifthenelse : js -> program -> program -> js
    val case : js -> (label * program) list -> program -> js
    val sequence : js -> js -> js
    val break : unit -> js
    val whileloop : js -> program -> js
    val continue : js
    val assign : ident -> js -> js
    val trycatch : program -> (ident * program) option -> js
    val skip : js
  end

  module Prim: sig
    val name : string -> js
    val apply_binop : string -> js * js -> js
    val apply_unary : string -> js -> js
    val instanceof : (js * js) -> js
  end
end

module CodeGen : CODEGEN = struct
  type js = PP.t
  type 'a t = (comp_unit * js, 'a) State.t
  type ident = Js.Ident.t
  type label = string
  type program = js list * js
  type fnkind = [`Regular | `Generator]

  module S = State
  let pure x = S.pure x
  let (>>=) m k = S.(m >>= k)

  let emit js' () =
    let open PP in
    S.get >>= fun (cu, js) ->
    S.put (cu, js $ js')

  let emit_runtime () =
    let input_line_opt ic =
      try Some (input_line ic)
      with End_of_file -> None
    in
    let read_lines ic =
      let rec aux acc =
        match input_line_opt ic with
        | Some line -> aux (line::acc)
        | None -> (List.rev acc)
      in
      aux []
    in
    let lines_of_file filename =
      let ic = open_in filename in
      let lines = read_lines ic in
      close_in ic; lines
    in
    let open Js in
    let open PP in
    S.get >>= fun (cu, js) ->
    let runtime =
      List.fold_left
        (fun acc file ->
          let js =
            text (String.concat "\n" (lines_of_file file))
          in
          acc
            $/ (vgrp
                  ((hgrp
                      (text (Printf.sprintf "/* [Begin] Include %s */" file)))
                      $/ js
                      $/ (hgrp (text (Printf.sprintf "/* [End] Include %s */" file))))))
        empty cu.includes
    in
    S.put (cu, js $ runtime)

  let persist _ = assert false

  let gen cu code =
    let open Js in
    let (_, st) = State.run ~init:(cu, PP.empty) code in
    let oc = open_out cu.target in
    Printf.fprintf oc "%s\n%!" (PP.to_string ~width:256 (PP.vgrp (snd st)));
    close_out oc

  let rec sequence : (js -> js) -> js list -> js
    = fun sep ->
      function
      | [] -> PP.empty
      | [x] -> sep x
      | x :: xs -> PP.((sep x) $/ (sequence sep xs))

  module Aux = struct
    let newline = PP.break
    let comment txt =
      let open PP in
      hgrp ((text "/*") $/ (text txt) $/ (text "*/"))

    let strict_mode = PP.(text "'use strict';")
  end

  let layout_program : (js list * js) -> js
    = fun (decls, stmt) ->
      let open PP in
      match decls with
      | [] -> vgrp (nest 2 stmt)
      | _ ->
         let decls' = sequence (fun d -> d) decls in
         vgrp (decls' $/ stmt)

  let layout_function : fnkind -> ident option -> ident list -> program -> js
    = fun kind binder params body ->
      let open PP in
      let fun_keyword =
        match kind with
        | `Regular -> "function"
        | `Generator -> "function*"
      in
      let optional_break, optional_name =
        match binder with
        | Some name -> break, (break $ (text name))
        | _ -> empty, empty
      in
      hgrp
         ((text fun_keyword)
             $  optional_name
             $  (text "(")
             $  (commalist ~f:(fun x -> text x) params)
             $  (text ")")
             $/ (text "{")
             $ (vgrp
                  (nest 2
                     (break
                        $ (layout_program body))))
             $ vgrp (break $ (text "}") $ optional_break))

  module Decl = struct
    let semi : js -> js
      = fun stmt ->
        PP.(stmt $ (text ";"))

    let let_binding kind binder expr =
      let kind =
        match kind with
        | `Const -> "const"
        | `Let   -> "let"
        | `Var   -> "var"
        | _      -> assert false
      in
      let open PP in
      semi
        (hgrp
           ((text kind) $/ (text binder) $/ (text "=") $/ expr)) $ break

    let fun_binding kind binder params prog =
      layout_function kind (Some binder) params prog
      (* let open PP in *)
      (* hgrp *)
      (*    ((text "function") *)
      (*        $/ (text binder) *)
      (*        $  (text "(") *)
      (*        $  (commalist ~f:(fun x -> text x) params) *)
      (*        $  (text ")") *)
      (*        $/ (text "{") *)
      (*        $ (vgrp *)
      (*             (nest 2 *)
      (*                (break *)
      (*                   $ (layout_program prog)))) *)
      (*        $ vgrp (break $ (text "}") $ break)) *)
  end

  module Expr = struct
    let var ident = PP.text ident
    let anon_fun kind params prog =
      layout_function kind None params prog
      (* let open PP in *)
      (* hgrp *)
      (*   ((text "function") *)
      (*       $  (text "(") *)
      (*       $  (commalist ~f:(fun x -> text x) params) *)
      (*       $  (text ")") *)
      (*       $/ (text "{") *)
      (*       $ (vgrp *)
      (*            (nest 2 *)
      (*               (break *)
      (*                  $ (layout_program prog)))) *)
      (*       $ vgrp (break $ (text "}"))) *)

    let apply f args =
      let open PP in
      hgrp
        (f $ (text "(") $ (commalist ~f:(fun x -> x) args) $ (text ")"))

    let access obj label =
      let open PP in
      let is_numeric label =
        try
          ignore @@ float_of_string label; true
        with _ -> false
      in
      let is_reserved label =
        List.mem
          label
          ["return";
           "default";
           "yield"]
      in
      if is_numeric label then (* use subscript notation *)
        hgrp (obj $ ((text (Printf.sprintf "[%s]" label))))
      else if  is_reserved label then (* use subscript notation *)
        hgrp (obj $ ((text (Printf.sprintf "['%s']" label))))
      else (* otherwise use dot notation *)
        hgrp (obj $ text "." $ (text label))

    let object' fields =
      let open PP in
      match fields with
      | [] -> text "{}"
      | _ ->
         hgrp
           ((text "{")
               $/ (commalist ~f:(fun (label,expr) -> (text (Printf.sprintf "\"%s\":" label)) $/ expr) fields)
               $/ text "}")

    let array elements =
      let open PP in
      match elements with
      | [||] -> text "[]"
      | _ ->
         hgrp
           ((text "[")
               $/ (commalist ~f:(fun expr -> expr) (Array.to_list elements))
               $/ text "]")

    let yield kind expr =
      let open PP in
      let yield =
        match kind with
        | `Star -> "yield*"
        | `Regular -> "yield"
      in
      hgrp ((text yield) $/ expr)

    let throw expr =
      let open PP in
      hgrp ((text "throw") $/ expr)

    let new' expr =
      let open PP in
      hgrp ((text "new" $/ expr))
  end

  module Stmt = struct
    let return e =
      let open PP in
      hgrp
        (Decl.semi ((text "return") $/ e))

    let expr e   = Decl.semi e
    let ifthenelse cond tt ff =
      let open PP in
      let ff =
        match ff with
        | ([], s) when s = empty -> empty
        | _ ->
           (text " else")
             $/ (text "{")
             $ (vgrp
                  (nest 0
                     (vgrp
                        (nest 2
                           (break
                              $ (layout_program ff))))
                     $/ (text "}")))
      in
      hgrp
        ((text "if")
            $/ (text "(")
            $ cond
            $ (text ")")
            $/ (text "{")
            $ (vgrp
                 (nest 0
                    (vgrp
                       (nest 2
                          (break
                             $ (layout_program tt))))
                    $/ (text "}")) $ ff))

    let case scrutinee cases default =
      let open PP in
      let cases =
        List.fold_left
          (fun acc (label, prog) ->
            (acc
               $/ (hgrp
                     ((text "case") $/ (text (Printf.sprintf "'%s': {" label))
                         $ (vgrp
                              (nest 2 (break $ (layout_program prog)))))))
              $/ (text "}")
          )
          PP.empty cases
      in
      let default =
        hgrp ((text "default: {")
                 $ (vgrp
                      (nest 2
                         (break $ (layout_program default)))))
          $/ (text "}")
      in
      hgrp
        ((text "switch")
            $/ (text "(")
            $ scrutinee
            $ (text ")")
            $/ (text "{")
            $ (vgrp (nest 2 (cases $/ default))))
        $/ (text "}")

    let sequence s1 s2 = PP.(vgrp (s1 $/ s2))

    let break () = PP.(text "break;")

    let whileloop cond body =
      let open PP in
      hgrp
        ((text "while")
            $/ (text "(")
            $ cond
            $ (text ")")
            $/ (text "{")
            $ (vgrp
                 (nest 0
                    (vgrp
                       (nest 2
                          (break $ (layout_program body))))
                    $/ (text "}"))))

    let continue = PP.(text "continue;")

    let assign x expr =
      let open PP in
      hgrp
        ((text x)
            $/ (text "=")
            $/ expr
         $ (text ";"))

    let trycatch m catch =
      let open PP in
      hgrp
        ((text "try")
         $/ (text "{")
         $ (vgrp
              (nest 0
                 (vgrp
                    (nest 2
                       (break $ (layout_program m))))
         $/ (text "}")))
         $/
         (match catch with
         | None -> empty
         | Some (ident, prog) ->
            (text (Printf.sprintf "catch(%s)" ident))
              $/ (text "{")
              $  (vgrp
                    (nest 0
                       (vgrp
                          (nest 2
                             (break $ (layout_program prog))))
                       $/ (text "}")))))

    let skip = PP.empty
  end

  module Prim = struct
    let name = PP.text

    let apply_binop op (x,y) =
      let open PP in
      hgrp ((text "(") $ x $/ (text op) $/ y $ (text ")"))

    let apply_unary op x =
      let open PP in
      hgrp ((text "(") $ (text op) $ x $ (text ")"))

    let instanceof (o, c) =
      let open PP in
      hgrp (o $/ (text "instanceof") $/ c)
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
       PP.(x $ (sequence (fun x -> x)) xs)
end

let is_primitive : string -> bool
  = fun p -> String.length p > 0 && p.[0] = '%'

let rec program' : Js.program -> (CodeGen.js list * CodeGen.js)
  = fun (decls, stmt) ->
    let decls = List.map (fun decl -> declaration decl) decls in
    decls, statement stmt

and declaration : Js.decl -> CodeGen.js
  = let open Js in
    function
    | DLet { bkind; binder; expr } ->
       CodeGen.Decl.let_binding bkind binder (expression expr)
    | DFun { fname = `Named binder; fkind; formal_params; body } ->
       CodeGen.Decl.fun_binding fkind binder formal_params (program' body)
    | _ -> assert false

and expression : Js.expression -> CodeGen.js
  = let open Js in
  function
  | EVar name -> CodeGen.Expr.var name
  | EFun { fname = `Anonymous; fkind; formal_params; body } ->
     let open CodeGen in
     Expr.anon_fun fkind formal_params (program' body)
  | EFun _ -> assert false
  | EApply (EPrim p, args) ->
     let open CodeGen.Prim in
     let open CodeGen.Expr in
     let pop1 : CodeGen.js list -> CodeGen.js
       = fun args -> List.hd args
     in
     let pop2 : CodeGen.js list -> CodeGen.js * CodeGen.js
       = fun args -> List.hd args, List.nth args 1
     in
     let args' = List.map expression args in
     begin match p with
     | "%eq" -> apply_binop "===" (pop2 args')
     | "%neq" -> apply_binop "!==" (pop2 args')
     | "%lt" -> apply_binop "<" (pop2 args')
     | "%gt" -> apply_binop ">" (pop2 args')
     | "%le" -> apply_binop "<=" (pop2 args')
     | "%ge" -> apply_binop ">=" (pop2 args')

     | "%int_add"  | "%float_add"  -> apply_binop "+" (pop2 args')
     | "%int_sub"  | "%float_sub"  -> apply_binop "-" (pop2 args')
     | "%int_mult" | "%float_mult" -> apply_binop "*" (pop2 args')
     | "%float_div"  -> apply_binop "/" (pop2 args')
     | "%assign" -> apply_binop "=" (pop2 args')
     | "%not" -> apply_unary "!" (pop1 args')
     | "%negate" -> apply_unary "-" (pop1 args')
     | "%noop" -> pop1 args'
     | "%instanceof" -> instanceof (pop2 args')

     | p when String.length p > 0 && String.get p 0 = '%' ->
        let ident = Printf.sprintf "_%s" (String.sub p 1 (String.length p - 1)) in
        apply (name ident) args'
     | _ ->
        apply (name p) args'
     end
  | EApply (f, args) ->
     let open CodeGen in
     Expr.apply (expression f) (List.map expression args)
  | EObj fields ->
     CodeGen.Expr.object' (List.map (fun (label, expr) -> label, expression expr) fields)
  | ELit l -> literal l
  | EAccess (obj, label) ->
     CodeGen.Expr.access (expression obj) label
  | EPrim p -> primitive p
  | EYield { ykind; yexpr } ->
     CodeGen.Expr.yield ykind (expression yexpr)
  | EArray arr ->
     CodeGen.Expr.array (Array.map expression arr)
  | EThrow expr ->
     CodeGen.Expr.throw (expression expr)
  | ENew expr ->
     CodeGen.Expr.new' (expression expr)

and statement : Js.statement -> CodeGen.js
  = let open Js in
  function
  | SReturn e -> CodeGen.Stmt.return (expression e)
  | SSeq (s1,s2) -> CodeGen.Stmt.sequence (statement s1) (statement s2)
  | SExpr e -> CodeGen.Stmt.expr (expression e)
  | SCase (scrutinee, cases, default) ->
     let cases =
       List.map
         (fun (label, prog) ->
           (label, program' prog))
         (Utility.StringMap.to_alist cases)
     in
     let default =
       match default with
       | None -> program' ([], SReturn (EApply (EPrim "%error", [ELit (LString "Match failure.")])))
       | Some prog -> program' prog
     in
     CodeGen.Stmt.case (expression scrutinee) cases default
  | SIf (cond,tt,ff) ->
     CodeGen.Stmt.ifthenelse (expression cond) (program' tt) (program' ff)
  | SBreak ->
     CodeGen.Stmt.break ()
  | SWhile (cond, body) ->
     CodeGen.Stmt.whileloop (expression cond) (program' body)
  | SAssign (x, expr) ->
     CodeGen.Stmt.assign x (expression expr)
  | SContinue ->
     CodeGen.Stmt.continue
  | STry (m, catch) ->
     let catch =
       match catch with
       | None -> None
       | Some (ident, prog) ->
          Some (ident, program' prog)
     in
     CodeGen.Stmt.trycatch (program' m) catch
  | SSkip -> CodeGen.Stmt.skip

and primitive : string -> CodeGen.js
  = fun p ->
  let ident = Printf.sprintf "_%s" (String.sub p 1 (String.length p - 1)) in
  CodeGen.Prim.name ident

and literal : Js.literal -> CodeGen.js =
  let open CodeGen.Prim in
  let open Js in
  function
  | LBool true -> name "true"
  | LBool false -> name "false"
  | LInt n -> name (string_of_int n)
  | LFloat f -> name (string_of_float f)
  | LString s -> name (Printf.sprintf "\"%s\"" s)
  | LChar c   -> name (Printf.sprintf "'%c'" c)


let emit : program:comp_unit -> unit -> unit
  = fun ~program () ->
    let open Js in
    let open CodeGen in
    let (decls, stmt) = program' program.program in
    let prog =
      pure () >>=
        emit (Aux.comment "Program generated...") >>=
        emit (Aux.newline) >>=
        emit (Aux.strict_mode) >>=
        emit (Aux.newline) >>=
        emit (Aux.newline) >>=
        emit_runtime >>=
        emit (Aux.newline) >>=
        emit (Aux.newline) >>=
        emit (Aux.comment "[Begin] Bindings") >>=
        emit (Aux.newline) >>=
        emit (sequence decls) >>=
        emit (Aux.newline) >>=
        emit (Aux.comment "[End] Bindings") >>=
        emit (Aux.newline) >>=
        emit (Aux.newline) >>=
        emit (Aux.comment "[Begin] Main computation") >>=
        emit (Aux.newline) >>=
        emit stmt >>=
        emit (Aux.newline) >>=
        emit (Aux.comment "[End] Main computation")
    in
    gen program prog
