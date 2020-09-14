module Scope = struct
  open Utility

  module StringMap = struct
    include StringMap

    (* Extends [m0] by [m1]. In case [m0] and [m1] overlap then the
       latter shadows the former. *)
    let extend : 'a t -> 'a t -> 'a t
      = fun m0 m1 ->
      if m0 == StringMap.empty then m1
      else if m1 == StringMap.empty || m0 == m1 then m0
      else StringMap.union (fun _ _ x -> Some x) m0 m1
  end

  type var = int
  type t = { vars: var StringMap.t }

  let empty : t = { vars = StringMap.empty }

  let bind_var : string -> var -> t -> t
    = fun name var env ->
    { vars = StringMap.add name var env.vars }

  let _extend : t -> t -> t
    = fun env env' ->
    { vars = StringMap.extend env.vars env'.vars }

  let lookup_var : string -> t -> var
    = fun name env ->
    StringMap.find name env.vars
end

(* Static name resolution. *)
module Resolve = struct

  let rec resolve scope' =
    let open Sugartypes in
    let open SourceCode in
    let open WithPos in
    let open CommonTypes in
    object(self : 'self_type)
      inherit SugarTraversals.map as super

      (* We maintain a reference to the current scope. *)
      val mutable scope = scope'

      (* For error reporting we maintain a reference to the source
         position of the current phrase node. *)
      val mutable phrase_position = Position.dummy

      (* Intended to be used by the object creator to extract the
         scope. *)
      method get_scope : Scope.t = scope

      (* Instantiates a fresh copy of this object with the current
         scope. *)
      method clone : 'self_type =
        resolve scope

      (* Creates a fresh variable identifier. *)
      method fresh_var : Var.var
        = Var.fresh_raw_var ()

      (* Binds a term variable in the current scope. *)
      method bind_var : string -> Var.var -> unit
        = fun name var ->
        scope <- Scope.bind_var name var scope

      method! binder : Binder.with_pos -> Binder.with_pos
        = fun bndr ->
        let name = Binder.to_name bndr in
        let var  = self#fresh_var in
        self#bind_var name var;
        Binder.set_var bndr var

      method resolve_var : Name.t -> Name.t
        = fun name ->
        try
          match name with
          | Name.Unresolved name ->
             Name.resolved name (Scope.lookup_var name self#get_scope)
          | _ -> assert false
        with Notfound.NotFound _ -> raise (Errors.unbound_variable phrase_position (Name.to_string name))

      method bindings : binding list -> binding list
        = fun bs -> List.map self#binding bs

      method! bindingnode = function
        | Val (pat, (qs, exp), loc, dt) ->
           let dt'  = self#option (fun o -> o#datatype') dt in
           (* It is important to resolve names in [exp] before
              visiting [pat] to ensure proper shadowing semantics of
              names. *)
           let exp' = self#phrase exp in
           let pat' = self#pattern pat in
           Val (pat', (qs, exp'), loc, dt')
        | Fun ({ fun_binder = bndr; fun_definition = (tvs, funlit); fun_signature = dt; _ } as fn)->
           let dt' = self#option (fun o -> o#datatype') dt in
           (* It is crucial to process the function binder [bndr]
              before processing the function body [funlit] as by
              default any function is assumed to be recursive. *)
           let bndr' = self#binder bndr in
           let funlit' = self#funlit funlit in
           Fun { fn with fun_binder = bndr';
                         fun_definition = (tvs, funlit');
                         fun_signature = dt' }
        | Funs fs ->
           (* Assumes mutual typenames have been processed already,
              which appears to be guaranteed by the parser. *)
           (* Two passes:
           1) First visit every function binder such that they are
              available in the function bodies.
           2) Resolve names in function bodies. *)
        let (fs' : recursive_function list) =
          List.fold_right
            (fun {node;pos} fs -> make ~pos { node with rec_binder = self#binder node.rec_binder } :: fs)
            fs []
        in
        let fs'' =
          List.fold_right
            (fun {node={ rec_definition = (tvs, funlit); rec_signature = dt; _ } as fn; pos} fs ->
              let dt' = self#option (fun o -> o#datatype') dt in
              let funlit' = self#funlit funlit in
              make ~pos { fn with rec_definition = (tvs, funlit'); rec_signature = dt' } :: fs)
            fs' []
        in
        Funs fs''
        | (Infix _) as node -> node
        | Exp exp -> Exp (self#phrase exp)
        | Typenames ts -> Typenames ts (* TODO(dhil): Name resolution
                                          for types. *)
        | Open  _
        | Import _
        | Module _ -> assert false (* TODO(dhil): For now we assume
                                      module stuff has been desugared
                                      by a previous pass. *)
        | Foreign alien ->
           let declarations =
             self#list
               (fun o (b, dt) ->
                 let dt = o#datatype' dt in
                 let b = o#binder b in
                 (b, dt))
               (Alien.declarations alien)
           in
           Foreign (Alien.modify ~declarations alien)
        | AlienBlock aliendecls ->
           let decls' =
             self#list
               (fun o (bndr, dt) ->
                 let dt' = o#datatype' dt in
                 let bndr' = o#binder bndr in
                 (bndr', dt'))
               Alien.(declarations aliendecls)
           in
           AlienBlock (Alien.modify ~declarations:decls' aliendecls)

      method cases : (Pattern.with_pos * phrase) list -> (Pattern.with_pos * phrase) list
        = fun cases ->
        List.map
          (fun (pat, body) ->
            let visitor = self#clone in
            let pat'  = visitor#pattern pat in
            let body' = visitor#phrase body in
            (pat', body'))
          cases

      method! phrase phr =
        let cur_pos = phrase_position in
        phrase_position <- pos phr;
        let phrasenode = self#phrasenode (node phr) in
        phrase_position <- cur_pos;
        WithPos.make ~pos:(pos phr) phrasenode

      method! phrasenode = function
        | Block (bs, body) ->
           (* Enters a new scope, which is thrown away on exit. *)
           let visitor = self#clone in
           let bs'= visitor#bindings bs in
           let body' = visitor#phrase body in
           Block (bs', body')
        | Var name ->
           (* Must be resolved. *)
           Var (self#resolve_var name)
        | QualifiedVar _names -> assert false (* TODO(dhil): Module
                                                 stuff is assumed to
                                                 be handled by an
                                                 earlier pass. *)
        | Escape (bndr, body) ->
           let visitor = self#clone in
           let bndr' = visitor#binder bndr in
           let body' = visitor#phrase body in
           Escape (bndr', body')
        | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
           let sh_expr = self#phrase sh_expr in
           let shd_params =
             self#option (fun o -> o#handle_params) sh_descr.shd_params
           in
           let sh_effect_cases = self#cases sh_effect_cases in
           let sh_value_cases = self#cases sh_value_cases in
           Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr = { sh_descr with shd_params } }
        | Switch (expr, cases, dt) ->
           let expr' = self#phrase expr in
           let cases' = self#cases cases in
           Switch (expr', cases', dt)
        | Receive (cases, dt) ->
           let cases' = self#cases cases in
           Receive (cases', dt)
        | FormBinding (body, pat) ->
           let visitor = self#clone in
           let body' = visitor#phrase body in
           let pat' = visitor#pattern pat in
           FormBinding (body', pat')
        | Offer (expr, cases, dt) ->
           let expr' = self#phrase expr in
           let cases' = self#cases cases in
           Offer (expr', cases', dt)
        | TryInOtherwise (expr, x, body, catch, dt) ->
           let expr' = self#phrase expr in
           let visitor = self#clone in
           let x' = visitor#pattern x in
           let body' = visitor#phrase body in
           let catch' = self#phrase catch in
           TryInOtherwise (expr', x', body', catch', dt)
        | CP cp_exp ->
           (* CP introduces a new scope. *)
           let visitor = self#clone in
           CP (visitor#cp_phrase cp_exp)
        | exp -> super#phrasenode exp
  end

  let program scope program = (resolve scope)#program program

  let sentence : Scope.t -> Sugartypes.sentence -> Sugartypes.sentence =
    fun _scope sentence ->
    let open Sugartypes in
    match sentence with
    | Definitions _bs     -> assert false
    | Expression _exp     -> assert false
    | (Directive _) as _d -> assert false
end

module Untyped = struct
  open Transform.Untyped

  let name = "resolveNames"

  let program state program =
    let program' = Resolve.program Scope.empty program in
    return state program'

  let sentence state sentence =
    let sentence' = Resolve.sentence Scope.empty sentence in
    return state sentence'
end
