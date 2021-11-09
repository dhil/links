open Utility
open Sugartypes
open CommonTypes
open SugarConstructors.DummyPositions
open SourceCode.WithPos

module TyEnv = Env.Name

(** TODO(dhil) FIXME: This transformation pass breaks (or abuses the
   lack of enforcement of) uniqueness of binders. This may very well
   cause problems later on. The proper fix is to parameterised the
   transformation pass by the current channel name. *)

let binder_of_name : Types.datatype -> Name.t -> Binder.with_pos
  = fun ty name ->
  match name with
  | Name.Unresolved name when Settings.get Basicsettings.Names.legacy_names ->
     Binder.make' ~name ~ty ()
  | Name.Local (name, var) when not (Settings.get Basicsettings.Names.legacy_names) ->
     let bndr = Binder.make' ~name ~ty ~fresh:false () in
     Binder.set_var bndr var
  | _ ->
     raise (Errors.desugaring_error ~pos:SourceCode.Position.dummy
              ~stage:Errors.DesugarCP ~message:"Incompatible name passed to 'binder_of_name'")

class desugar_cp compenv env =
  let open PrimaryKind in
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | CP p ->
       let rec desugar_cp = fun o {node = p; _} ->
         match p with
         | CPUnquote (bs, e) ->
            let envs = o#backup_envs in
            let (o, bs) = TransformSugar.listu o (fun o -> o#binding) bs in
            let (o, e, t) = o#phrase e in
            let o = o#restore_envs envs in
            o, block_node (bs, e), t
         | CPGrab ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            let wait = Compenv.Lib.canonical_name "wait" compenv in
            o, block_node
                ([val_binding (any_pat dp) (fn_appl_var wait c)],
                 with_dummy_pos e), t
         | CPGrab ((c, Some (Types.Input (_a, s), grab_tyargs)), Some bndr, p) -> (* FYI: a = u *)
            let envs = o#backup_envs in
            let venv =
              let x = Binder.to_name' bndr in
              let u = Binder.to_type bndr in
              TyEnv.bind x u (o#get_var_env ())
              |> TyEnv.bind c s
            in
            let o = {< var_env = venv >} in
            let (o, e, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            let receive = Compenv.Lib.canonical_name "receive" compenv in
            o, block_node
                 ([val_binding (with_dummy_pos (
                                    Pattern.Record ([("1", variable_pat' bndr);
                                                     ("2", variable_pat' (binder_of_name s c))], None)))
                               (fn_appl receive grab_tyargs [var c])],
                 with_dummy_pos e), t
         | CPGive ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            let close = Compenv.Prelude.canonical_name "closeBang" compenv in
            o, block_node
                ([val_binding (any_pat dp) (fn_appl_var close c)],
                 with_dummy_pos e), t
         | CPGive ((c, Some (Types.Output (_t, s), give_tyargs)), Some e, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind c s (o#get_var_env ()) >} in
            let (o, e, _typ) = o#phrase e in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            let send = Compenv.Lib.canonical_name "send" compenv in
            o, block_node
                ([val_binding (variable_pat' (binder_of_name s c))
                              (fn_appl send give_tyargs [e; var c])],
                 with_dummy_pos p), t
         | CPGiveNothing c ->
            o, Var c, TyEnv.find c (o#get_var_env ())
         | CPSelect (c, label, p) ->
            let s = TyEnv.find c (o#get_var_env ()) in
            let s' = TypeUtils.select_type label s in
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind c s' (o#get_var_env ()) >} in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, block_node
                 ([val_binding (variable_pat' (binder_of_name s' c))
                     (with_dummy_pos (Select (label, var c)))],
                  with_dummy_pos p), t
         | CPOffer (c, cases) ->
            let s = TyEnv.find c (o#get_var_env ()) in
            let desugar_branch (label, p) (o, cases) =
              let envs = o#backup_envs in
              let s' = TypeUtils.choice_at label s in
              let o = {< var_env = TyEnv.bind c s' (o#get_var_env ()) >} in
              let (o, p, t) = desugar_cp o p in
              let pat : Pattern.with_pos =
                with_dummy_pos (Pattern.Variant (label, Some (variable_pat' (binder_of_name s' c))))
              in
              o#restore_envs envs, ((pat, with_dummy_pos p), t) :: cases in
            let (o, cases) = List.fold_right desugar_branch cases (o, []) in
            (match List.split cases with
                | (_, []) -> assert false (* Case list cannot be empty *)
                | (cases, t :: _ts) ->
                    o, Offer (var c, cases, Some t), t)
         | CPLink (c, d) ->
            let ct = TyEnv.find c (o#get_var_env ()) in
            let link_sync = Compenv.Prelude.canonical_name "linkSync" compenv in
            o, fn_appl_node link_sync [(Type, ct); (Row, o#lookup_effects)]
                 [var c; var d],
            Types.make_endbang_type
         | CPComp (bndr, left, right) ->
            let c = Binder.to_name' bndr in
            let s = Binder.to_type bndr in
            let envs = o#backup_envs in
            let (o, left, _typ) = desugar_cp {< var_env = TyEnv.bind c s (o#get_var_env ()) >} left in
            let (o, right, t) = desugar_cp {< var_env = TyEnv.bind c (Types.dual_type s) (o#get_var_env ()) >} right in
            let o = o#restore_envs envs in
            let effect_row =
              Types.flatten_row o#lookup_effects
              |> Types.remove_field Types.wild
              |> (if Settings.get Basicsettings.Sessions.exceptions_enabled
                  then Types.remove_field Value.session_exception_operation
                  else identity)
            in
            let left_block =
              let accept = Compenv.Lib.canonical_name "accept" compenv in
              let close = Compenv.Prelude.canonical_name "closeBang" compenv in
              spawn Angel NoSpawnLocation
                (block ([ val_binding (variable_pat' bndr) (fn_appl_var accept c);
                          val_binding (variable_pat' (Binder.set_type bndr Types.make_endbang_type))
                            (with_dummy_pos left)],
                        fn_appl_var close c))
                ~row:effect_row
            in
            let o = o#restore_envs envs in
            let new' = Compenv.Lib.canonical_name "new" compenv in
            let request = Compenv.Lib.canonical_name "request" compenv in
            o, block_node
                 ([val_binding (variable_pat' (Binder.set_type bndr (Types.Application (Types.access_point, [(Type, s)]))))
                     (fn_appl new' [] []);
                   val_binding (any_pat dp) left_block;
                   val_binding (variable_pat' (Binder.set_type bndr (Types.dual_type s)))
                     (fn_appl_var request c)],
                  with_dummy_pos right), t
         | _ -> assert false in
       desugar_cp o p
    | e -> super#phrasenode e
end

let desugar_cp compenv env = ((new desugar_cp compenv env) : desugar_cp :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make'(struct
        let name = "cp"
        let obj compenv env = (desugar_cp compenv env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)

