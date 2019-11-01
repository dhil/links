open Utility
open CommonTypes
open Sugartypes
open SugarConstructors.DummyPositions
open SourceCode.WithPos

let raise_invalid_element pos =
  let open Errors in
  raise (desugaring_error ~pos ~stage:DesugarPages
    ~message:"Invalid element in page literal")

let closed_wild = Types.make_singleton_closed_row ("wild", `Present Types.unit_type)

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
let rec desugar_page (o, page_type) =
  let desugar_nodes : phrase list -> phrase = function
    | [] ->
       let unit_p = assert false (* "unitP" *) (* TODO FIXME reference to unitP *)
       var unit_p
    | page :: ps ->
       let page = desugar_page (o, page_type) page in
       List.fold_left (fun prev page ->
           let page = desugar_page (o, page_type) page in
           fn_appl "joinP" [`Row (o#lookup_effects)] [prev; page])
       page ps
  in
    fun ({node=e; pos} as phrase) ->
      match e with
        | _ when is_raw phrase ->
          (* TODO: check that e doesn't contain any formletplacements or page placements *)
           fn_appl "bodyP" [`Row (o#lookup_effects)] [phrase]
        | FormletPlacement (formlet, handler, attributes) ->
            let (_, formlet, formlet_type) = o#phrase formlet in
            let formlet_type = Types.concrete_type formlet_type in
            let a = Types.fresh_type_variable (lin_any, res_any) in
            let b = Types.fresh_type_variable (lin_any, res_any) in
            Unify.datatypes (`Alias (("Formlet", [(PrimaryKind.Type, default_subkind)], [`Type a]), b), formlet_type); (* TODO FIXME reference to Formlet *)
            let form_p = assert false (* "formP" *) in (* TODO FIXME reference to formP *)
            fn_appl form_p
              [`Type a; `Row (o#lookup_effects)]
              [formlet; handler; attributes]
        | PagePlacement (page) -> page
        | Xml ("#", [], _, children) ->
            desugar_nodes children
        | Xml (name, attrs, dynattrs, children) ->
           let xb = o#fresh_binder Types.xml_type "xml" in
           let x = o#refer_to xb in
            fn_appl "plugP" [`Row (o#lookup_effects)]
               [fun_lit ~args:[Types.make_tuple_type [Types.xml_type], closed_wild]
                        dl_unl [[variable_pat xb]]
                        (xml name attrs dynattrs [block ([], var x)]);
                desugar_nodes children]
        | _ -> raise_invalid_element pos

and desugar_pages env =
object
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | Page e ->
       let (o, e, _t) = super#phrase e in
       let env = Context.typing_environment (o#get_context ()) in
       let page_type = Instantiate.alias "Page" [] env.Types.tycon_env in (* TODO FIXME reference to Page *)
       let e = desugar_page (o, page_type) e in
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

module Typeable
  = Transform.Typeable.Make(struct
        let name = "pages"
        let obj env = (desugar_pages env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
