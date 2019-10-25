open CommonTypes
open SourceCode
open SourceCode.WithPos
open Sugartypes
open SugarConstructors.DummyPositions

(*
 try { M } as (pat) in { N } otherwise { N' }

  --->

 handle M with {
   Return pat -> N
   _SessionFail _ _ -> N'
 }

*)


module TyEnv = Env.String

let failure_op_name = Value.session_exception_operation

let extension_guard pos =
  let exceptions_enabled =
    Basicsettings.Sessions.exceptions_enabled
  in
  let get_setting_name () =
    Settings.get_name exceptions_enabled
  in
  Settings.get Basicsettings.Sessions.exceptions_enabled
  || raise (Errors.disabled_extension ~pos ~setting:(get_setting_name (), true) "Session exceptions")

class insert_toplevel_handlers context =
object (o: 'self_type)
  inherit (TransformSugar.transform context) as super

  method! phrasenode = function
    | (Spawn (Wait, _, _, _)) as sw ->
        super#phrasenode sw
    | Spawn (_, _, _, None) -> assert false
    | Spawn (k, spawn_loc, ({node=body; pos} as body_phr), Some inner_effects) ->
        (* First, bind the body result to a dummy variable. *)
        let (_, _, body_dt) = o#phrasenode body in
        let unit_phr = with_dummy_pos (RecordLit ([], None)) in
        let with_pos = fun node -> SourceCode.WithPos.make ~pos node in
        let dshb = o#fresh_binder body_dt "dsh" in
        let ignore_pat = with_pos (Pattern.Variable dshb) in
        let body =
          with_pos (Block
            ([ with_pos (Val (ignore_pat, ([], body_phr), Location.Unknown, None))], unit_phr)) in

        (* Next, we need to construct a try-as-in-otherwise with the above body *)
        let as_var = Utility.gensym ~prefix:"spawn_aspat" () in
        let asb = o#fresh_binder Types.unit_type as_var in
        let as_pat = variable_pat asb in
        let (o, spawn_loc) = o#given_spawn_location spawn_loc in
        let envs = o#backup_envs in
        (* Now, process body using inner effects *)
        let outer_effects = o#lookup_effects in
        let process_type = `Application (Types.process, [`Row inner_effects]) in
        let o = o#with_effects inner_effects in
        let (o, body, _body_dt) = o#phrase body in
        (* Restore outer effects *)
        let o = o#with_effects outer_effects in
        let body =
          TryInOtherwise (body, as_pat,
                          var (o#refer_to asb), unit_phr, Some (Types.unit_type)) in
        let o = o#restore_envs envs in
        (o, Spawn (k, spawn_loc, with_dummy_pos body, Some inner_effects), process_type)
    | e -> super#phrasenode e
end


class desugar_session_exceptions env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrase = function
    | { node = Raise; pos } when extension_guard pos ->
        (* Compile `raise` as the following:
         * switch (do SessionFail) { }
         *
         * (where (do SessionFail) has the empty type) *)
        let ty =
          Types.fresh_type_variable (CommonTypes.lin_any, CommonTypes.res_any) in
        let doOp = DoOperation (failure_op_name, [], Some (Types.empty_type)) in
        let with_pos x = SourceCode.WithPos.make ~pos x in
        (o, with_pos (Switch (with_pos doOp, [], Some ty)), ty)
    | { node = TryInOtherwise (_, _, _, _, None); _} -> assert false
    | { node = TryInOtherwise (try_phr, pat, as_phr, otherwise_phr, (Some dt)); pos }
      when extension_guard pos ->
        let (o, try_phr, try_dt) = o#phrase try_phr in
        let envs = o#backup_envs in
        let (o, pat) = o#pattern pat in
        let (o, as_phr, _as_dt) = o#phrase as_phr in
        let o = o#restore_envs envs in
        let (o, otherwise_phr, otherwise_dt) = o#phrase otherwise_phr in
        (* Now, to create a handler... *)

        let return_clause = (pat, as_phr) in
        (* Otherwise clause: Distinguished 'session failure' name. Since
         * we'll never use the continuation (and this is invoked after pattern
         * deanonymisation in desugarHandlers), generate a fresh name for the
         * continuation argument. *)
        let outer_effects = o#lookup_effects in

        let fail_cont_ty =
          Types.make_pure_function_type [] (Types.empty_type) in

        let inner_effects =
          effect_row
            |> Types.row_with (failure_op_name, `Present fail_cont_ty)
            |> Types.flatten_row in

        let contb = o#fresh_binder (Types.make_function_type [] inner_effects Types.empty_type) "dsh" in
        let cont_pat = variable_pat contb in

        let otherwise_pat : Sugartypes.Pattern.with_pos =
          with_dummy_pos (Pattern.Effect (failure_op_name, [], cont_pat)) in

        let otherwise_clause = (otherwise_pat, otherwise_phr) in

        let value_cases = [return_clause] in
        let effect_cases = [otherwise_clause] in

        (* Manually construct a row with the two hardwired handler cases. *)
        let raw_row = Types.row_with ("Return", (`Present try_dt)) inner_effects in
        (* Dummy types *)
        let types =
          (inner_effects, try_dt, outer_effects, otherwise_dt) in

        let hndl_desc = {
          shd_depth = Deep;
          shd_types = types;
          shd_raw_row = raw_row;
          shd_params = None;
        } in

        let hndlr = {
          sh_expr = try_phr;
          sh_effect_cases = effect_cases;
          sh_value_cases = value_cases;
          sh_descr = hndl_desc
        } in (o, SourceCode.WithPos.make ~pos (Handle hndlr), dt)
    | e -> super#phrase e
end


(*
 * The "naive" typing rule for try-in-otherwise is unsound
 * as it allows possibly-linear variables to be used twice:
 *
 *
 *   G1 |- L : A   G2, x : A |- M : B    G2 |- N : B
 *   -------------------------------------------------
 *      G1, G2 |- try L as x in M otherwise N : B
 *
 * Instead, the typechecker implements a more restrictive typing
 * rule which only allows a single variable in the typing of the
 * success continuation:
 *
 *   G |- L : A   x : A |- M : B     |- N : B
 *   ------------------------------------------
 *     G |- try L as x in M otherwise N : B
 *
 * Thankfully, the full power of the first rule can be restored
 * through the use of a simple macro-translation, performed by the
 * wrap_linear_handlers function:
 *
 * try L as x in M otherwise N
 *   -->
 * switch (try L as x in Just(x) otherwise Nothing) {
 *   case Just(x) -> M
 *   case Nothing -> N
 * }
 *)
let wrap_linear_handlers context =
  object
    inherit SugarTraversals.map as super
    method! phrase = function
      | {node=TryInOtherwise (l, x, m, n, dtopt); _} ->
         let comp_unit = Context.compilation_unit context in
         let try_xb =
           WithPos.make (Binder.make ~host:comp_unit ~name:"try_x" ()) in
         with_dummy_pos
           (Switch (
                with_dummy_pos
                  (TryInOtherwise
                     (super#phrase l,
                      variable_pat try_xb,
                      constructor ~body:(var "try_x") "Just", (* TODO FIXME reference to try_x. *)
                      constructor "Nothing", dtopt)),
                [ (with_dummy_pos (Pattern.Variant ("Just", (Some x))), super#phrase m)
                ; (with_dummy_pos (Pattern.Variant ("Nothing", None)), super#phrase n) ]
                , None))
      | p -> super#phrase p
  end

let _insert_toplevel_handlers env =
  ((new insert_toplevel_handlers env) :
    insert_toplevel_handlers :> TransformSugar.transform)

let desugar_session_exceptions env =
  ((new desugar_session_exceptions env) :
    desugar_session_exceptions :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make(struct
        let name = "session_exceptions (typeable)"
        let obj env = (desugar_session_exceptions env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
module Untyped = struct
  open Transform.Untyped
  let name = "session_exceptions (untyped)"
  let program state program =
    let program' = (wrap_linear_handlers state)#program program in
    return state program'

  let sentence state sentence =
    let sentence' = (wrap_linear_handlers state)#sentence sentence in
    return state sentence'
end
