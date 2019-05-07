(* Desugaring of modules.

   Eliminates modules from the AST by prefixing names. *)

open Utility
open Sugartypes
open SourceCode.WithPos

type t =
  { vars: unit StringMap.t;
    types: unit StringMap.t;
    mutable next: int;
    prefix: string }

let fresh prefix =
  { prefix;
    next = 0;
    vars = StringMap.empty;
    types = StringMap.empty }

let empty = fresh ""

let next : t -> int
  = fun st ->
  let next = st.next in
  st.next <- next + 1; next

let expand : t -> name -> name
  = fun st name ->
  if st.prefix <> "" then
    let id = next st in
    Printf.sprintf "%s.%s$%d" st.prefix name id
  else
    name

let desugar loadstore _comp_unit_name program =
  let rec desugar st =
    object(self)
      inherit SugarTraversals.map as super

      method! binding = function
        | b -> super#binding b

      method bindings = function
        | [] -> []
        | { node = Import _; _ } :: bs ->
           self#bindings bs
        (* | { node = QualifiedImport _; _ } :: bs ->
         *    self#bindings bs *)
        | { node = Module _; _ } :: bs ->
           self#bindings bs
        | b :: bs ->
           super#binding b :: self#bindings bs
    end
  in
  (desugar empty)#program program
