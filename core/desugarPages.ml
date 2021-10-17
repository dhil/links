open Utility
open CommonTypes
open Sugartypes
open SugarConstructors.DummyPositions
open SourceCode.WithPos

let raise_invalid_element pos =
  let open Errors in
  raise (desugaring_error ~pos ~stage:DesugarPages
    ~message:"Invalid element in page literal")

let rec is_raw phrase =
  match phrase.node with
  | TextNode _ -> true
  | Block _ -> true
  | FormletPlacement _
  | PagePlacement _ -> false
  | Xml (_, _, _, children) ->
     List.for_all is_raw children
  | _e -> raise_invalid_element phrase.pos

(* DODGEYNESS:

   The first argument to desugar_page is an object which is only used
   to lookup effecs and to construct formlet types.

   This code assumes that:

     - the effecs are the same throughout the page literal
     - the environment is unchanged after calling o#phrase formlet
*)
let rec desugar_page compenv (o, page_type) =
  let desugar_nodes : phrase list -> phrase = function
    | [] -> var (Compenv.Prelude.canonical_name "unitP" compenv)
    | page :: ps ->
       let page = desugar_page compenv (o, page_type) page in
       List.fold_left (fun prev page ->
           let page = desugar_page compenv (o, page_type) page in
           let join_p = Compenv.Prelude.canonical_name "joinP" compenv in
           fn_appl join_p [(PrimaryKind.Row, o#lookup_effects)] [prev; page])
       page ps
  in
    fun ({node=e; pos} as phrase) ->
      match e with
        | _ when is_raw phrase ->
           (* TODO: check that e doesn't contain any formletplacements or page placements *)
           let body_p = (Compenv.Prelude.canonical_name "bodyP" compenv) in
           fn_appl body_p [(PrimaryKind.Row, o#lookup_effects)] [phrase]
        | FormletPlacement (formlet, handler, attributes) ->
           let open PrimaryKind in
           let (_, formlet, formlet_type) = o#phrase formlet in
           let formlet_type = Types.concrete_type formlet_type in
           let a = Types.fresh_type_variable (lin_any, res_any) in
           let b = Types.fresh_type_variable (lin_any, res_any) in
           let form_p = Compenv.Prelude.canonical_name "formP" compenv in
           Unify.datatypes (Types.Alias (("Formlet", [(Type, default_subkind)], [(Type, a)], false), b), formlet_type);
           fn_appl form_p [(Type, a); (Row, o#lookup_effects)] [formlet; handler; attributes]
        | PagePlacement (page) -> page
        | Xml ("#", [], _, children) ->
            desugar_nodes children
        | Xml (name, attrs, dynattrs, children) ->
           let x = Utility.gensym ~prefix:"xml" () in
           let bndr = Binder.make' ~name:x ~ty:Types.xml_type () in
           let x = Binder.to_name' bndr in
           let plug_p = Compenv.Prelude.canonical_name "plugP" compenv in
           fn_appl plug_p [(PrimaryKind.Row, o#lookup_effects)]
             [fun_lit ~args:[Types.make_tuple_type [Types.xml_type], Types.closed_wild_row]
                dl_unl [[variable_pat' bndr]]
                (xml name attrs dynattrs [block ([], var x)]);
              desugar_nodes children]
        | _ -> raise_invalid_element pos

and desugar_pages compenv env =
object
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | Page e ->
        let (o, e, _t) = super#phrase e in
        let page_type = Instantiate.alias "Page" [] env.Types.tycon_env in
        let e = desugar_page compenv (o, page_type) e in
          (o, e.node, page_type)
    | e -> super#phrasenode e
end

let is_pageless =
object
  inherit SugarTraversals.predicate as super

  val pageless = true
  method satisfied = pageless

  method! phrasenode = function
    | Page _ -> {< pageless = false >}
    | e -> super#phrasenode e
end

module Typeable = struct
  open Transform.Typeable

  let name = "pages"

  let program state program =
    let compenv = Context.compilation_environment (context state) in
    let (_, program', ty) = (desugar_pages compenv (Context.typing_environment (context state)))#program program in
    return (with_type ty state) program'

  let sentence state sentence =
    let compenv = Context.compilation_environment (context state) in
    let (_, sentence', ty) = (desugar_pages compenv (Context.typing_environment (context state)))#sentence sentence in
    return (with_type ty state) sentence'
end
