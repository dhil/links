(* Desugaring of modules.

   Eliminates modules from the AST by prefixing names... *)

(* ... aka a poor person's name resolution. *)
(* I hope this code can be repurposed for a proper name resolution
   pass later. *)

open Utility
open Sugartypes
open SourceCode.WithPos

(* TODO FIXME: use ropes rather than strings to build names. *)
module Baptiser = struct
  type t =
    { mutable next: int;
      prefix: (int * string) list }

  let empty : t =
    { next = 0; prefix = [] }

  (* Persistent *)
  let remember name st =
    let next = st.next in
    st.next <- next + 1;
    { empty with prefix = (next, name) :: st.prefix }

  (* Stateful *)
  let baptise st name =
    let next = st.next in
    st.next <- next + 1;
    let components =
      List.fold_left
        (fun suffix (i, name) ->
          (Printf.sprintf "%s%d" name i) :: suffix)
         [] ((next, name) :: st.prefix)
    in
    String.concat "$" components
end

(* The following data structures model scopes. *)
type scope =
  { modules: module_member StringMap.t;
    typenames: type_member StringMap.t;
    terms: term_member StringMap.t }
and module_member = scope
and type_member = string
and term_member = string
and t = { inner: scope; outer: scope }

let empty_scope =
  { modules = StringMap.empty;
    typenames = StringMap.empty;
    terms = StringMap.empty }

let empty = { inner = empty_scope; outer = empty_scope }

let rec spaces n buffer =
  if n > 0
  then (Buffer.add_char buffer ' '; spaces (n - 1) buffer)
  else ()

let put buffer s = Buffer.add_string buffer s
let nl buffer = Buffer.add_char buffer '\n'

(* let rec string_of_scope buffer indent { modules; typenames; terms } =
 *   spaces indent buffer;
 *   put buffer "modules = {";
 *   nl buffer;
 *   StringTrie.iter
 *     (fun prefix item ->
 *       spaces (indent + 1) buffer;
 *       put buffer (Printf.sprintf "[%s]:" (String.concat ";" prefix));
 *       nl buffer;
 *       let _ = string_of_scope buffer (indent + 2) item in
 *       nl buffer)
 *     modules;
 *   nl buffer;
 *   spaces indent buffer;
 *   put buffer "}\n";
 *   spaces indent buffer;
 *   put buffer "typenames = {";
 *   nl buffer;
 *   StringTrie.iter
 *     (fun prefix item ->
 *       spaces (indent + 2) buffer;
 *       put buffer (Printf.sprintf "[%s] -> %s" (String.concat ";" prefix) item);
 *       nl buffer)
 *     typenames;
 *   spaces indent buffer;
 *   put buffer "}\n";
 *   spaces indent buffer;
 *   put buffer "terms = {";
 *   nl buffer;
 *   StringTrie.iter
 *     (fun prefix item ->
 *       spaces (indent + 2) buffer;
 *       put buffer (Printf.sprintf "[%s] -> %s" (String.concat ";" prefix) item);
 *       nl buffer)
 *     terms;
 *   spaces indent buffer;
 *   put buffer "}"; buffer
 * and string_of_scopes { outer; inner } =
 *   let buffer = Buffer.create 32 in
 *   put buffer "outer = {";
 *   nl buffer;
 *   let buffer = string_of_scope buffer 1 outer in
 *   nl buffer;
 *   put buffer "}\n";
 *   put buffer "inner = {";
 *   nl buffer;
 *   let buffer = string_of_scope buffer 1 inner in
 *   nl buffer; Buffer.contents buffer *)

let shadow : scope -> scope -> scope
  = fun scope mask ->
  let select_mask _ _ mask = Some mask in
  let modules =
    StringMap.union select_mask scope.modules mask.modules
  in
  let typenames =
    StringMap.union select_mask scope.typenames mask.typenames
  in
  let terms =
    StringMap.union select_mask scope.terms mask.terms
  in
  { modules; typenames; terms }

let open_module : scope -> t -> t
  = fun module_scope scopes ->
  let inner = shadow scopes.inner module_scope in
  { scopes with inner }

(* let import_module : name -> scope -> t -> t
 *   = fun module_name module_scope scopes ->
 *   assert false *)

(* We do not produce an error if a name fails to resolve, which
   happens if a variable is unbound. We defer error handling to the
   type checker. We produce a "best guess" of its name, which is
   simply its qualified form. *)
let best_guess : name list -> name
  = String.concat "."

let rec resolve_module : name list -> scope -> scope
  = fun names scope ->
  match names with
  | [] -> assert false
  | [name] -> StringMap.find name scope.modules
  | prefix :: names -> resolve_module names (StringMap.find prefix scope.modules)

let resolve_module : name list -> t -> scope
  = fun names scopes ->
  try resolve_module names scopes.inner
  with Notfound.NotFound _ ->
    resolve_module names scopes.outer (* Allow any errors propagate. *)

let rec resolve_qualified_var : name list -> scope -> name
  = fun names scope ->
  match names with
  | [] -> assert false
  | [name] -> StringMap.find name scope.terms
  | prefix :: names ->
     resolve_qualified_var names (StringMap.find prefix scope.modules)
  (* try StringTrie.find names scope.inner.terms
   * with Notfound.NotFound _ ->
   *   try StringTrie.find names scope.outer.terms
   *   with Notfound.NotFound _ ->
   *     (\* Printf.printf "Resolution for [%s] failed! Dumping contents...\n%!" (String.concat ";" names);
   *      * Printf.printf "%s\n%!" (string_of_scopes scope); *\)
   *     best_guess names *)

let resolve_qualified_var : name list -> t -> name
  = fun names scopes ->
  try resolve_qualified_var names scopes.inner
  with Notfound.NotFound _ ->
    try resolve_qualified_var names scopes.outer
    with Notfound.NotFound _ -> best_guess names

let rec resolve_qualified_typename : name list -> scope -> name
  = fun names scope ->
  match names with
  | [] -> assert false
  | [name] -> StringMap.find name scope.typenames
  | prefix :: names ->
     resolve_qualified_typename names (StringMap.find prefix scope.modules)

let resolve_qualified_typename : name list -> t -> name
  = fun names scopes ->
  try resolve_qualified_typename names scopes.inner
  with Notfound.NotFound _ ->
    try resolve_qualified_typename names scopes.outer
    with Notfound.NotFound _ -> best_guess names

let resolve_var : name -> t -> name
  = fun name scopes -> resolve_qualified_var [name] scopes

let resolve_typename : name -> t -> name
  = fun name scopes -> resolve_qualified_typename [name] scopes

let enter_module : name -> scope -> t -> t
  = fun module_name module_scope scope ->
  (* Printf.printf "Enter module %s\n%!" module_name; *)
  (* dump module_scope; dump scope; *)
  (* let modules =
   *   try let scope' = resolve_module [module_name] scope in
   *       shadow scope' module_scope
   *   with Notfound.NotFound _ -> module_scope
   * in *)
  let modules' = StringMap.add module_name module_scope scope.inner.modules in
  { scope with inner = { scope.inner with modules = modules' } }

let enter_term : name -> string -> t -> t
  = fun term_name prefixed_name scope ->
  let terms = StringMap.add term_name prefixed_name scope.inner.terms in
  { scope with inner = { scope.inner with terms = terms } }

let enter_typename : name -> string -> t -> t
  = fun typename prefixed_name scope ->
  let typenames = StringMap.add typename prefixed_name scope.inner.typenames in
  { scope with inner = { scope.inner with typenames = typenames } }

let renew : t -> t
  = fun scopes ->
  let outer = shadow scopes.outer scopes.inner in
  { outer; inner = empty_scope }

let rec desugar_module : Baptiser.t -> t -> Sugartypes.binding -> binding list * t
  = fun baptiser scope binding ->
  match binding.node with
  | Module (name, bs) ->
     let visitor = desugar ~toplevel:true (Baptiser.remember name baptiser) (renew scope) in
     let bs'    = visitor#bindings bs in
     let scope' = visitor#get_scope in
     let scope'' = enter_module name scope'.inner scope in
     (* Printf.printf "enter_module before:\n%s\n%!" (string_of_scopes scope);
      * Printf.printf "enter_module after:\n%s\n%!" (string_of_scopes scope''); *)
     (bs', scope'')
  | _ -> assert false
and desugar ?(toplevel=false) (baptiser : Baptiser.t) (scope : t) =
  let open Sugartypes in
  object(self : 'self_type)
    inherit SugarTraversals.map as super

    val scope : t ref = ref scope
    val baptiser : Baptiser.t ref = ref baptiser
    method get_scope = !scope

    method type_binder : name -> name
      = fun name ->
      (* Construct a prefixed name for [name]. *)
      let name' = Baptiser.baptise !baptiser name in
      self#bind_type name name'; name'

    method! binder : Binder.with_pos -> Binder.with_pos
      = fun bndr ->
      let name = Binder.to_name bndr in
      let name' = Baptiser.baptise !baptiser name in
      self#bind_term name name';
      Binder.set_name bndr name'

    method bind_term name name' =
      Printf.printf "Binding [%s] -> %s\n%!" (String.concat ";" [name]) name';
      scope := enter_term name name' !scope

    method bind_type name name' =
      scope := enter_typename name name' !scope

    method open_module pos path =
      try
        (* Printf.printf "Before opening %s:\n%s\n%!" (String.concat "." path) (string_of_scopes !scope); *)
        let module_scope = resolve_module path !scope in
        (* Printf.printf "BEFORE SHADOW\n%!";
         * Printf.printf "=== Scope 1\n%!";
         * dump !scope;
         * Printf.printf "=== Scope 2\n%!";
         * dump scope';
         * scope := shadow !scope scope';
         * Printf.printf "AFTER SHADOW\n%!";
         * dump !scope *)
        scope := open_module module_scope !scope;
        (* Printf.printf "After opening %s:\n%s\n%!" (String.concat "." path) (string_of_scopes !scope); *)
      with Notfound.NotFound _ ->
        raise (Errors.module_error ~pos (Printf.sprintf "Unbound module %s" (best_guess path)))

    method! patternnode = let open Pattern in function
      | Variable bndr ->
      (* Affects [scope]. Global binders in patterns are always
         qualified. *)
        let name = Binder.to_name bndr in
        let name' = Baptiser.baptise !baptiser name in
        self#bind_term name name';
        Variable (Binder.set_name bndr name')
      (* | (Variable bndr) as node ->
       *  (\* Affects [scope]. Local binders in patterns are always
       *     unqualified. *\)
       *   let name = Binder.to_name bndr in
       *   self#bind_term name name; node *)
      | As (bndr, pat) ->
       (* Affects [scope]. *)
        let name = Binder.to_name bndr in
        let bndr' =
          if toplevel
          then Binder.set_name bndr (Baptiser.baptise !baptiser name)
          else bndr
        in
        let name' = Binder.to_name bndr' in
        self#bind_term name name';
        As (bndr', self#pattern pat)
      | p -> super#patternnode p

    method! funlit : funlit -> funlit
      = fun (paramss, body) ->
      let visitor = desugar ~toplevel:false !baptiser !scope in
      let paramss' =
        List.map
          (fun params ->
            List.map (fun param -> visitor#pattern param) params)
          paramss
      in
      let body' = visitor#phrase body in
      (paramss', body')

    method! phrasenode = function
      | Block (bs, body) ->
       (* Enters a new scope, which is thrown away on exit. *)
        let visitor = desugar ~toplevel:false !baptiser !scope in
        let bs'= visitor#bindings bs in
        let body' = visitor#phrase body in
        Block (bs', body')
      | Var name ->
      (* Must be resolved. *)
         Printf.printf "Resolving %s\n%!" name;
         Var (resolve_var name !scope)
      | QualifiedVar names ->
      (* Must be resolved. *)
         (* Printf.printf "Resolving [%s]\n%s\n%!" (String.concat ";" names) (string_of_scopes !scope); *)
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
       (* It is important to process [bndr] before processing
          [funlit] as functions are allowed to call themselves. *)
        let bndr' = self#binder bndr in
        let dt' = self#option (fun o -> o#datatype') dt in
        let funlit' = self#funlit funlit in
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
              (self#binder bndr, lin, lit, loc, dt, pos) :: fs)
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
       (* It is important to process [body] before [pat] to avoid
          inadvertently bringing the binder(s) in [pat] into the
          scope of [body]. *)
         let body' = self#phrase body in
         let pat' = self#pattern pat in
         let dt' = self#option (fun o -> o#datatype') dt in
         Val (pat', (tvs, body'), loc, dt')
      | Foreign (bndr, raw_name, lang, ext_file, dt) ->
         let dt' = self#datatype' dt in
         let bndr' = self#binder bndr in
         Foreign (bndr', raw_name, lang, ext_file, dt')
      | AlienBlock (lang, lib, decls) ->
         let decls' =
           self#list
             (fun o (bndr, dt) ->
               let dt' = o#datatype' dt in
               let bndr' = o#binder bndr in
               (bndr', dt'))
             decls
         in
         AlienBlock (lang, lib, decls')
      | Module _ | Import _ | Open _ -> assert false (* Should have been processed by this point. *)
      | b -> super#bindingnode b

    method bindings = function
      | [] -> []
      (* TODO FIXME: Imports should bring a synthetic module into
         scope. *)
      | { node = Import names; pos } :: bs
      | { node = Open names; pos } :: bs ->
      (* Affects [scope]. *)
         self#open_module pos names; self#bindings bs
      | ({ node = Module (_name, _); _ } as module') :: bs ->
      (* Affects [scope] and hoists [bs'] *)
         Printf.printf "Desugar module %s\n%!" _name;
         let bs', scope' = desugar_module !baptiser !scope module' in
         (* Printf.printf "Before desugar %s:\n%s\n%!" name (string_of_scopes !scope);
          * Printf.printf "After desugar %s:\n%s\n%!" name (string_of_scopes scope'); *)
         scope := scope'; bs' @ self#bindings bs
      | b :: bs ->
         let b = self#binding b in
         b :: self#bindings bs

    method! program (bs, exp) =
      let bs' = self#bindings bs in
      (bs', self#option (fun o -> o#phrase) exp)
  end

let desugar : Sugartypes.program -> Sugartypes.program
  = fun program ->
  Printf.fprintf stderr "Before elaboration:\n%s\n%!" (Sugartypes.show_program program);
  let result = (desugar ~toplevel:true Baptiser.empty empty)#program program in
  Printf.fprintf stderr "After elaboration:\n%s\n%!" (Sugartypes.show_program result);
  result
