(** This pass checks the syntactic well-formedness of
   patterns. Computation/effect/operations patterns (henceforth just
   'effect patterns') are only allowed to occur at the "top-level" of
   a pattern (technically an effect pattern is allowed to occur inside
   a tuple, but this fact is opaque to the end user). *)

open Sugartypes
open SourceCode.WithPos

let checker =
    object (o)
      inherit SugarTraversals.fold as super

      val in_handler = false
      val toplevel_pattern = true

      method! phrasenode = function
        | Handle { sh_exprs = ms; sh_effect_cases = eff_cases;
                   sh_value_cases = val_cases; sh_descr = descr } ->
          let arity = List.length ms in
          let _ = o#list (fun o -> o#phrase) ms in
          let _ =
            o#option (fun o -> o#handle_params) descr.shd_params
          in
          let _ = o#list
              (fun _o (p, e) ->
                 let _ = o#check_value_case_pattern arity p in
                 {< in_handler = false >}#phrase e)
              val_cases
          in
          let _ = o#list
              (fun o { ec_pattern; ec_resumption; ec_body } ->
                 let _ = match node ec_pattern with
                   | Pattern.Tuple ps when arity > 1 ->
                     o#list (fun _o -> {< in_handler = true; toplevel_pattern = true >}#pattern) ps
                   | Pattern.Operation _ ->
                     {< in_handler = true; toplevel_pattern = true >}#pattern ec_pattern
                   | _ -> o#pattern ec_pattern
                 in
                 ignore (o#check_resumption_pattern ec_resumption);
                 {< in_handler = false; toplevel_pattern = true >}#phrase ec_body)
              eff_cases
          in o
        | p -> super#phrasenode p

      method! pattern pat =
        let o' = {< toplevel_pattern = false >} in
        match node pat with
        | Pattern.Operation (_label, p, resume) ->
          if not in_handler
          then raise (Errors.effect_pattern_outside_handler (pos pat))
          else if not toplevel_pattern
          then raise (Errors.effect_pattern_below_toplevel (pos pat))
          else let _ = o'#pattern p in
               o'#check_resumption_pattern resume
        | _ -> o'#super_pattern pat

      method super_pattern pat = super#pattern pat

      method check_value_case_pattern arity pat =
        let rec check_nary pat' =
          match node pat' with
          | Pattern.Any | Pattern.Variable _ | Pattern.Tuple _ -> ()
          | Pattern.HasType (p, _) | Pattern.As (_, p) -> check_nary p
          | _ -> raise (Errors.illformed_nary_value_pattern (pos pat))
        in
        let _ = {< in_handler = true >}#pattern pat in
        if arity = 1 then o
        else (check_nary pat; o)

      method check_resumption_pattern pat =
        let rec check pat' =
          match node pat' with
          | Pattern.Any -> ()
          | Pattern.Variable _ -> ()
          | Pattern.HasType (p, _) | Pattern.As (_, p) -> check p
          | _ -> raise (Errors.illformed_resumption_pattern (pos pat))
        in
        check pat; o
    end

module Untyped = struct
  open Transform.Untyped

  let name = "check_effect_patterns"

  let program state program =
    ignore (checker#program program);
    return state program

  let sentence state sentence =
    ignore (checker#sentence sentence);
    return state sentence
end
