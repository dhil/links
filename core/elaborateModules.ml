(* Desugaring of modules.

   Eliminates modules from the AST by prefixing names... *)

(* ... aka a poor person's name resolution. *)
(* I hope this code can be repurposed for a proper name resolution
   pass later. *)

open Utility
open Sugartypes
open SourceCode.WithPos

(* TODO FIXME: use ropes rather than strings to build names. *)
module Prefix = struct
  type t = string

  let to_string : t -> string
    = fun x -> x

  let of_string : string -> t
    = fun x -> x

  let add_suffix : t -> string -> t
    = fun prefix suffix -> prefix ^ suffix

  let empty : t = ""

  let length : t -> int
    = String.length
end

(* The following data structures model scopes. *)
type t =
  { modules: module_member StringTrie.t;
    typenames: type_member StringTrie.t;
    terms: term_member StringTrie.t }
and module_member = t
and type_member = string
and term_member = string

let empty =
  { modules = StringTrie.empty;
    typenames = StringTrie.empty;
    terms = StringTrie.empty }

(* We do not produce an error if a name fails to resolve, which
   happens if a variable is unbound. We defer error handling to the
   type checker. We produce a "best guess" of its name, which is
   simply its qualified form. *)
let best_guess : name list -> name
  = String.concat "."

let resolve_module : name list -> t -> t
  = fun names { modules; _ } ->
  StringTrie.find names modules

let resolve_qualified_var : name list -> t -> name
  = fun names { terms; _ } ->
  try StringTrie.find names terms
  with Notfound.NotFound _ -> best_guess names

let resolve_qualified_typename : name list -> t -> name
  = fun names { typenames; _ } ->
  try StringTrie.find names typenames
  with Notfound.NotFound _ -> best_guess names

let resolve_var : name -> t -> name
  = fun name scope -> resolve_qualified_var [name] scope

let resolve_typename : name -> t -> name
  = fun name scope -> resolve_qualified_typename [name] scope

let desugar (program : Sugartypes.program) =
  let open Sugartypes in
  let desugar =
    object(self)
      inherit SugarTraversals.map as super

      val next : int ref = ref 0
      val scope : t ref = ref empty
      val prefix = ref Prefix.empty

      method backup = (!scope, !prefix, !next)
      method restore (scope', prefix', next') =
        scope := scope';
        next := next';
        prefix := prefix'

      method type_binder : name -> name
        = fun _name -> assert false
      method term_binder : Binder.with_pos -> Binder.with_pos
        = fun _bndr -> assert false
      method module_binder : name -> (t * string * int) -> (t * string * int)
        = fun name (scope', prefix, next) ->
        ({ !scope with modules = StringTrie.add [name] scope' !scope.modules }, prefix, next)

      method bind_term names name =
        scope := { !scope with terms = StringTrie.add names name !scope.terms }

      method bind_type names name =
        scope := { !scope with typenames = StringTrie.add names name !scope.typenames }

      (* method bind_module names module' =
       *   scope := { !scope with modules = StringTrie.add names module' !scope.modules } *)

      (* Every binder should be processed by one of the subsequent methods. *)
      method! binder _ = assert false

      method! patternnode = let open Pattern in function
        | (Variable bndr) as node ->
         (* Affects [scope]. Binders in variable patterns are always
            unqualified. *)
          let name = Binder.to_name bndr in
          self#bind_term [name] name; node
        | As (bndr, pat) ->
         (* Affects [scope]. Binders in as patterns are always
            unqualified. *)
          let name = Binder.to_name bndr in
          self#bind_term [name] name;
          As (bndr, self#pattern pat)
        | p -> super#patternnode p

      method! phrasenode = function
        | Block (bs, body) ->
         (* Introduces a new scope *)
          let st = self#backup in
          let bs' = self#bindings bs in
          let body' = self#phrase body in
          let () = self#restore st in
          Block (bs', body')
        | Var name ->
        (* Must be resolved. *)
          Var (resolve_var name !scope)
        | QualifiedVar names ->
        (* Must be resolved. *)
          Var (resolve_qualified_var names !scope)
        | p -> super#phrasenode p

      method! datatypenode = let open Datatype in function
        | TypeApplication (name, args) ->
        (* Must be resolved. *)
          let args' = self#list (fun o -> o#type_arg) args in
          TypeApplication (resolve_typename name !scope, args')
        | QualifiedTypeApplication (names, args) ->
        (* Must be resolved. *)
          let args' = self#list (fun o -> o#type_arg) args in
          TypeApplication (resolve_qualified_typename names !scope, args')
        | dt -> super#datatypenode dt

      method! bindingnode = function
        | Fun (bndr, lin, (tvs, funlit), loc, dt) ->
          let dt' = self#option (fun o -> o#datatype') dt in
          let funlit' = self#funlit funlit in
          let bndr' = self#term_binder bndr in
          Fun (bndr', lin, (tvs, funlit'), loc, dt')
        | Funs fs ->
          (* Assumes mutual typenames have been processed already,
             which appears to be guaranteed by the parser. *)
          (* Two passes:
             1) Register all the names such that they are available
              inside of each function body.
             2) Process the function bodies. *)
           let (fs' : recursive_function list) =
             List.fold_right
               (fun (bndr, lin, lit, loc, dt, pos) fs ->
                 (self#term_binder bndr, lin, lit, loc, dt, pos) :: fs)
               fs []
           in
           let fs'' =
             List.fold_right
               (fun (bndr, lin, (tvs, funlit), loc, dt, pos) fs ->
                 let dt' = self#option (fun o -> o#datatype') dt in
                 let funlit' = self#funlit funlit in
                 (bndr, lin, (tvs, funlit'), loc, dt', pos) :: fs)
               fs' []
           in
           Funs fs''
        | Typenames ts ->
          (* Must be processed before any mutual function bindings in
             the same mutual binding group. *)
          (* Same procedure as above. *)
           let ts' =
             List.fold_right
               (fun (name, tyvars, dt, pos) ts ->
                 (self#type_binder name, tyvars, dt, pos) :: ts)
               ts []
           in
           let ts'' =
             List.fold_right
               (fun (name, tyvars, dt, pos) ts ->
                 let dt' = self#datatype' dt in
                 (name, tyvars, dt', pos) :: ts)
               ts' []
           in
           Typenames ts''
        | Val (pat, (tvs, body), loc, dt) ->
          let pat' = self#pattern pat in
          let body' = self#phrase body in
          let dt' = self#option (fun o -> o#datatype') dt in
          Val (pat', (tvs, body'), loc, dt')
        | Foreign (bndr, raw_name, lang, ext_file, dt) ->
          let dt' = self#datatype' dt in
          let bndr' = self#term_binder bndr in
          Foreign (bndr', raw_name, lang, ext_file, dt')
        | AlienBlock (lang, lib, decls) ->
          let decls' =
            self#list
              (fun o (bndr, dt) ->
                (* TODO process [bndr]. *)
                let dt' = o#datatype' dt in
                let bndr' = o#term_binder bndr in
                (bndr', dt'))
              decls
          in
          AlienBlock (lang, lib, decls')
        | Module _
        | Import _
        | Open _ -> assert false (* Should have been processed by this point. *)
        | b -> super#bindingnode b

      method bindings = function
        | [] -> []
        (* TODO FIXME: Imports should be bring a synthetic module into scope. *)
        | { node = Import _names; _ } :: bs
        | { node = Open _names; _ } :: bs ->
         (* Affects [scope]. *)
          self#bindings bs
        | { node = Module (name, bs'); _ } :: bs ->
          (* Affects [scope] and hoists [bs'] *)
          let st = self#backup in
          let bs'' = self#bindings bs' in
          self#restore (self#module_binder name st);
          bs'' @ self#bindings bs
        | b :: bs ->
          super#binding b :: self#bindings bs

      method! program (bs, exp) =
       (self#bindings bs, self#option (fun o -> o#phrase) exp)
    end
  in
  desugar#program program
