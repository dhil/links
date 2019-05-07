(* Desugaring of modules.

   Eliminates modules from the AST by prefixing names... *)

(* ... aka a poor person's name resolution. *)
(* I hope this code can be repurposed for a proper name resolution
   pass later. *)

open Utility
open Sugartypes
open SourceCode.WithPos

(* The following data structures model scopes. *)
(* TODO FIXME: use ropes rather than strings to build names. *)
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
  let desugar (current : t) (_global : t) =
    object(self)
      inherit SugarTraversals.map as super

      method type_binder : name -> name
        = fun _name -> assert false
      method term_binder : Binder.with_pos -> Binder.with_pos
        = fun _bndr -> assert false

      (* Every binder should be processed by one of the subsequent methods. *)
      method! binder _ = assert false

      method! patternnode = let open Pattern in function
        | Variable bndr ->
        (* Affects [current]. *)
          Variable (self#term_binder bndr)
        | As (bndr, pat) ->
        (* Affects [current] *)
          As (self#term_binder bndr, self#pattern pat)
        | p -> super#patternnode p

      method! phrasenode = function
        | Block (_bs, _body) ->
        (* Introduces a new scope *)
          assert false
        | Var name ->
        (* Must be resolved. *)
          Var (resolve_var name current)
        | QualifiedVar names ->
        (* Must be resolved. *)
          Var (resolve_qualified_var names current)
        | p -> super#phrasenode p

      method! datatypenode = let open Datatype in function
        | TypeApplication (name, args) ->
        (* Must be resolved. *)
          let args' = self#list (fun o -> o#type_arg) args in
          TypeApplication (resolve_typename name current, args')
        | QualifiedTypeApplication (names, args) ->
        (* Must be resolved. *)
          let args' = self#list (fun o -> o#type_arg) args in
          TypeApplication (resolve_qualified_typename names current, args')
        | dt -> super#datatypenode dt

      method! bindingnode = function
        | Fun (bndr, lin, (tvs, funlit), loc, dt) ->
          let dt' = self#option (fun o -> o#datatype') dt in
          let funlit' = self#funlit funlit in
          let bndr' = self#term_binder bndr in
          Fun (bndr', lin, (tvs, funlit'), loc, dt')
        | Funs _fs ->
          (* Assumes mutual typenames have been processed already,
             which appears to be guaranteed by the parser. *)
          assert false
        | Typenames _ts ->
          (* Must be processed before any mutual function bindings in
             the same mutual binding group. *)
          assert false
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
        (* Affects [current]. *)
          self#bindings bs
        | { node = Module (_name, bs'); _ } :: bs ->
        (* Affects [global] and hoists [bs'] *)
          let bs'' = self#bindings bs' in
          bs'' @ self#bindings bs
        | b :: bs ->
          super#binding b :: self#bindings bs

      method! program (bs, exp) =
       (self#bindings bs, self#option (fun o -> o#phrase) exp)
    end
  in
  (desugar empty empty)#program program
