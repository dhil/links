open CommonTypes
open Utility
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

class insert_toplevel_handlers env =
object (o: 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | (Spawn (Wait, _, _, _)) as sw ->
        super#phrasenode sw
    | Spawn (_, _, _, None) -> assert false
    | Spawn (k, spawn_loc, ({node=body; pos} as body_phr), Some inner_effects) ->
        (* First, bind the body result to a dummy variable. *)
        let (_, _, body_dt) = o#phrasenode body in
        let unit_phr = with_dummy_pos (RecordLit ([], None)) in
        let with_pos = fun node -> SourceCode.WithPos.make ~pos node in
        let ignore_pat = with_pos (Pattern.Variable (
          with_pos <|
            Binder.make
              ~name:(Utility.gensym ~prefix:"dsh" ())
              ~ty:body_dt ())) in
        let body =
          with_pos (Block
            ([ with_pos (Val (ignore_pat, ([], body_phr), Location.Unknown, None))], unit_phr)) in

        (* Next, we need to construct a try-as-in-otherwise with the above body *)
        let as_var = Utility.gensym ~prefix:"spawn_aspat" () in
        let as_pat = variable_pat ~ty:(Types.unit_type) as_var in
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
                          var as_var, unit_phr, Some (Types.unit_type)) in
        let o = o#restore_envs envs in
        (o, Spawn (k, spawn_loc, with_dummy_pos body, Some inner_effects), process_type)
    | e -> super#phrasenode e
end


class desugar_session_exceptions env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrase = function
    | { node = Raise; pos } ->
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
    | { node = TryInOtherwise (try_phr, pat, as_phr, otherwise_phr, (Some dt)); pos } ->
        let (o, try_phr, try_dt) = o#phrase try_phr in
        let envs = o#backup_envs in
        let (o, pat) = o#pattern pat in
        let (o, as_phr, _as_dt) = o#phrase as_phr in
        let o = o#restore_envs envs in
        let (o, otherwise_phr, otherwise_dt) = o#phrase otherwise_phr in
        (* Now, to create a handler... *)
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

        let cont_pat = variable_pat ~ty:(Types.make_function_type [] inner_effects (Types.empty_type))
          (Utility.gensym ~prefix:"dsh" ()) in

        let otherwise_pat : Sugartypes.Pattern.with_pos =
          with_dummy_pos (Pattern.Operation { label = failure_op_name; parameters = []; resumption = Some cont_pat }) in

        let cases =
          [ { patterns = [pat]; resumption = None; body = as_phr }; (* return clause *)
            { patterns = [otherwise_pat]; resumption = Some cont_pat; body = otherwise_phr }]
        in

        (* Manually construct a row with the two hardwired handler cases. *)
        let raw_row = Types.row_with ("Return", (`Present try_dt)) inner_effects in
        (* Dummy types *)
        let types =
          (inner_effects, try_dt, outer_effects, otherwise_dt) in

        let descriptor = {
          shd_depth = Shallow;
          shd_types = types;
          shd_raw_row = raw_row;
          shd_params = None;
        } in

        (o, SourceCode.WithPos.make ~pos (Handle { expressions = [try_phr]; cases; descriptor }), dt)
    | e -> super#phrase e
end


let contains_session_exceptions prog =
  let o =
    object
      inherit SugarTraversals.predicate as super
      val has_exceptions = false
      method satisfied = has_exceptions

      method! phrasenode = function
        | TryInOtherwise _
        | Raise -> {< has_exceptions = true >}
        | p -> super#phrasenode p
    end in
  (o#program prog)#satisfied


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
let wrap_linear_handlers =
  object
    inherit SugarTraversals.map as super
    method! phrase = function
      | {node=TryInOtherwise (l, x, m, n, dtopt); _} ->
          let fresh_var = Utility.gensym ?prefix:(Some "try_x") () in
          let fresh_pat = variable_pat fresh_var in
          with_dummy_pos
          (Switch (
            with_dummy_pos
             (TryInOtherwise
              (super#phrase l,
               fresh_pat,
               constructor ~body:(var fresh_var) "Just",
               constructor "Nothing", dtopt)),
            [
              (with_dummy_pos (Pattern.Variant ("Just", (Some x))), super#phrase m);
              (with_dummy_pos (Pattern.Variant ("Nothing", None)), super#phrase n)
            ], None))
      | p -> super#phrase p
  end

let settings_check prog =
  if not (contains_session_exceptions prog) then () else
  if not (Settings.get_value Basicsettings.Sessions.exceptions_enabled) then
    raise (
      Errors.settings_error
        ("File contains session exceptions but session_exceptions not enabled. " ^
                "Please set 'session_exceptions' configuration flag to true."))
  else if not (Settings.get_value Basicsettings.Handlers.enabled) then
    raise (
      Errors.settings_error
        ("File contains session exceptions, which require handlers, " ^
         " but handlers are not enabled. " ^
         "Please set 'enable_handlers' configuration flag to true."))
  else ()

let insert_toplevel_handlers env =
  ((new insert_toplevel_handlers env) :
    insert_toplevel_handlers :> TransformSugar.transform)

let desugar_session_exceptions env =
  ((new desugar_session_exceptions env) :
    desugar_session_exceptions :> TransformSugar.transform)

let desugar_program : TransformSugar.program_transformer =
  fun env program -> snd3 ((desugar_session_exceptions env)#program program)


let show prog =
  Printf.printf "%s\n\n" (Sugartypes.show_program prog);
  prog
