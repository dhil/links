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
type label = string
and arguments = expression list
and expression =
  | EVar       of Ident.t
  | EApply     of expression * arguments (* e(e* ) *)
  | ESubscript of expression * label     (* e[l] *)
  | EFun       of fn                     (* function(ident* ) { stmt } *)
  | EConstant  of constant
  | EPrim      of primitive
  | EObj       of (label * expression) list
and statement =
  | SIf of expression * statement * statement  (* if (expr) { stmt1 } else { stmt2 } *)
  | SCase of expression * (constant * statement) * statement option (* switch (expr) { case c1: stmt1 break; ... case cN: stmtN break; [default: stmt] } *)
  | SReturn of expression (* return e; *)
  | SSeq of statement * statement (* stmt1; stmt2 *)
  | SBind of [`Const | `Let | `Var] * Ident.t * expression (* [ const | let | var ] x = e; *)
  | SFun of fn
  | SExpr of expression
and constant =
  | CBool of bool
  | CInt of int
  | CFloat of float
  | CString of string
  | CChar of char
and primitive =
  | PFun of string
  | PVar of string
and program = statement list
and fn = {
  kind: [`Named of string | `Anonymous];
  formal_params: Ident.t list;
  body: statement;
}
