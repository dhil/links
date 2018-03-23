(*pp deriving *)
open Utility

(** {0 Code generation} *)

module Symbols =
struct
  let words =
    CharMap.from_alist
      [ '!', "bang";
        '$', "dollar";
        '%', "percent";
        '&', "and";
        '*', "star";
        '+', "plus";
        '/', "slash";
        '<', "lessthan";
        '=', "equals";
        '>', "greaterthan";
        '?', "huh";
        '@', "monkey";
        '\\', "backslash";
        '^', "caret";
        '-', "hyphen";
        '.', "fullstop";
        '|', "pipe";
        '_', "underscore"]

  let js_keywords= ["break";"else";"new";"var";"case";"finally";"return";"void";
                    "catch";"for";"switch";"while";"continue";"function";"this";
                    "with";"default";"if";"throw";"delete";"in";"try";"do";
                    "instanceof";"typeof";
                    (* "future keywords" *)
                    "abstract";"enum";"int";"short";"boolean";"export";
                    "interface";"static";"byte";"extends";"long";"super";"char";
                    "final";"native";"synchronized";"class";"float";"package";
                    "throws";"const";"goto";"private";"transient";"debugger";
                    "implements";"protected";"volatile";
                   ]

  let has_symbols name =
(*    not (Lib.is_primitive name) && *)
      List.exists (not -<- Utility.Char.isWord) (explode name)

  let wordify name =
    if has_symbols name then
      ("_" ^
         mapstrcat "_"
         (fun ch ->
            if (Utility.Char.isWord ch) then
              String.make 1 ch
            else if CharMap.mem ch words then
              CharMap.find ch words
            else
              failwith("Internal error: unknown symbol character: "^String.make 1 ch))
         (Utility.explode name))
        (* TBD: it would be better if this split to chunks maximally matching
           (\w+)|(\W)
           then we would not split apart words in partly-symbolic idents. *)
    else if List.mem name js_keywords then
      "_" ^ name (* FIXME: this could conflict with Links names. *)
    else name

end

(** Generate a JavaScript name from a binder *)
let name_binder (x, info) =
  let (_, name, _) = info in
  if String.length name = 0 then
    "_" ^ string_of_int x
  else
    (* Closure conversion means we can no longer rely on top-level
       functions having unique names *)
    (* match scope with *)
    (* | `Local -> *)
      if (Str.string_match (Str.regexp "^_g[0-9]") name 0) then
        "_" ^ string_of_int x (* make the generated names slightly less ridiculous in some cases *)
      else
        Symbols.wordify name ^ "_" ^ string_of_int x
    (* | `Global -> Symbols.wordify name *)

(** Generate a JavaScript name from a variable number. *)
let var_name_var x = "_" ^ string_of_int x

(** Generate a JavaScript name from a binder based on the unique
    integer for that binder. *)
let var_name_binder (x, _) = var_name_var x

(* Compilation unit *)
type nenv = int Env.String.t
type tenv = Types.typing_environment
type envs = {
  tenv: tenv;
  nenv: nenv;
}

type 'a comp_unit =
  { source: string;
    target: string;
    program: 'a;
    envs: envs;
    includes: string list; }

let make_comp_unit : ?includes:string list -> ?source:string -> ?target:string -> program:'a -> nenv:nenv -> tenv:tenv -> unit -> 'a comp_unit
  = fun ?(includes=[]) ?(source="dummy.links") ?(target="a.js") ~program ~nenv ~tenv () ->
    { source; target; program; envs = { tenv; nenv; }; includes }

(* JS Binder generation *)
module Ident = struct
  type t = string
  let make : ?prefix:string -> unit -> t
    = fun ?(prefix="_") () ->
      Utility.gensym ~prefix ()

  let of_binder : Var.binder -> t
    = fun b ->
      Printf.sprintf "%s_%d" (Var.name_of_binder b) (Var.var_of_binder b)

  let of_string : string -> t
    = fun s -> s
end

(* Js IR *)
type literal =
  | LBool of bool
  | LInt of int
  | LFloat of float
  | LString of string
  | LChar of char
      deriving (Show)

module LitMap = Map.Make(struct
  type t = literal
  let compare a b =
    let cmp = Pervasives.compare in
    match a, b with
    | LBool x, LBool y -> cmp x y
    | LInt x, LInt y -> cmp x y
    | LFloat x, LFloat y -> cmp x y
    | LString x, LString y -> cmp x y
    | LChar x, LChar y -> cmp x y
    | LBool _, _   -> 1 | _, LBool _   -> (-1)
    | LChar _, _   -> 1 | _, LChar _   -> (-1)
    | LInt _, _    -> 1 | _, LInt _    -> (-1)
    | LFloat _, _  -> 1 | _, LFloat _  -> (-1)
    (* | LString _, _ -> 1 | _, LString _ -> (-1) *)

  module Show_t = Show_literal
end)

type 'a litmap = 'a StringMap.t

type label = string
and arguments = expression list
and expression =
  | EVar       of Ident.t
  | EApply     of expression * arguments (* e(e* ) *)
  | EAccess    of expression * label     (* e[l] *)
  | EFun       of fn                     (* function(ident* ) { stmt } *)
  | ELit       of literal
  | EPrim      of string
  | EObj       of (label * expression) list
  | EArray     of expression array
  | EYield     of yield
  | EThrow     of expression
  | ENew       of expression
and statement =
  | SIf of expression * program * program  (* if (expr) { stmt1 } else { stmt2 } *)
  | SCase of expression * program litmap * program option (* switch (expr) { case c1: stmt1 break; ... case cN: stmtN break; [default: stmt] } *)
  | SReturn of expression (* return e; *)
  | SSeq of statement * statement (* stmt1; stmt2 *)
  (* | SBind of [`Const | `Let | `Var] * Ident.t * expression (\* [ const | let | var ] x = e; *\) *)
  (* | SFun of fn *)
  | SExpr of expression
  | SBreak
  | SWhile of expression * program
  | SContinue
  | SAssign of Ident.t * expression
  | STry of program * (Ident.t * program) option (* try M catch(e) { M } *)
  | SSkip
and decl =
  | DLet of binding
  | DFun of fn
and program = (decl list * statement)
and fn = {
  fname: [`Named of string | `Anonymous];
  fkind: [`Regular | `Generator];
  formal_params: Ident.t list;
  body: program;
}
and binding = {
  bkind: [`Const | `Let | `Var];
  binder: Ident.t;
  expr: expression;
}
and yield = {
  ykind: [`Star | `Regular];
  yexpr: expression;
}

let rec sequence : statement list -> statement = function
  | [stmt] -> stmt
  | stmt :: stmts -> SSeq (stmt, sequence stmts)
  | [] -> failwith "Sequence empty list."


let rec replace_return r (decls, stmt) =
  let rr = replace_return r in
  let stmt =
    match stmt with
    | SReturn e
    | SExpr e ->
       SAssign (r, e)
    | SWhile (e, p) ->
       SWhile (e, rr p)
    | STry (p, catch) ->
       STry (rr p, catch)
    | SIf (cond, tt, ff) ->
       SIf (cond, rr tt, rr ff)
    | SCase (scrutinee, name_map, default) ->
       let name_map =
         StringMap.map rr name_map
       in
       let default =
         match default with
         | None -> None
         | Some p -> Some (rr p)
       in
       SCase (scrutinee, name_map, default)
    | SSeq (s1, s2) ->
       let (_, s1) = rr ([], s1) in
       let (_, s2) = rr ([], s2) in
       SSeq (s1, s2)
    | s -> s
  in
  (* let decl = function *)
  (*   | DFun { body; _ } as d -> *)
  (*      { d with body = rr body } *)
  (*   | d -> d *)
  (* in *)
  decls, stmt

let rec eliminate_thunks (decls, stmt) =
  let rec bindings = function
    | [] ->
       [], statement stmt
    | DLet {
      binder;
      expr = EApply(EFun { fname = `Anonymous; formal_params = []; body; _ }, []);
      _ } :: bs ->
       let decl =
         DLet {
           bkind = `Let;
           binder;
           expr = EVar "undefined";
         }
       in
       let (decls, body) = replace_return binder body in
       let (decls', body') = bindings bs in
       decl :: decls @ decls', SSeq (body, body')
    | (DFun ({ body; _ } as d)) :: bs->
       let decls, stmt = bindings bs in
       (DFun { d with body = eliminate_thunks body }) :: decls, stmt
    | b :: bs ->
       let decls, stmt = bindings bs in
       b :: decls, stmt
  and statement = function
    | s -> s
  in
  bindings decls
