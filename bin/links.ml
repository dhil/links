open Links_core
open Performance
open Utility

module BS = Basicsettings


(** Ensure the settings were parsed correctly *)
let _ = ParseSettings.validate_settings ()



let to_evaluate : string list ref = ParseSettings.to_evaluate
let file_list : string list ref = ParseSettings.file_list

let print_simple rtype value =
  print_string (Value.string_of_value value);
  print_endline
    (if Settings.get_value (BS.printing_types) then
          " : " ^ Types.string_of_datatype rtype
        else
          "")

let process_filearg prelude envs file =
  let result = Driver.NonInteractive.run_file prelude envs file in
  print_simple result.Driver.result_type result.Driver.result_value

let process_exprarg envs expr =
  let result = Driver.NonInteractive.evaluate_string_in envs expr in
  print_simple result.Driver.result_type result.Driver.result_value

let main () =
  let prelude, envs = measure "prelude" Driver.NonInteractive.load_prelude () in

  for_each !to_evaluate (process_exprarg envs);
    (* TBD: accumulate type/value environment so that "interact" has access *)

  for_each !file_list (process_filearg prelude envs);
  let should_start_repl = !to_evaluate = [] && !file_list = [] in
  if should_start_repl then
    begin
      print_endline (Settings.get_value BS.welcome_note);
      Repl.interact envs
    end


(* Compiling to JavaScript *)
let compile_js () =
  let module Eval = Evalir.Eval(Webserver.Webserver) in
  let load_prelude () =
    let open Loader in
    let source =
      Errors.display
         (lazy (Loader.load_file (Lib.nenv, Lib.typing_env) (Settings.get_value BS.prelude_file)))
    in
    let (nenv, tyenv) = source.envs in
    let (globals, _, _) = source.program in

    let tyenv = Lib.patch_prelude_funs tyenv in

    Lib.prelude_tyenv := Some tyenv;
    Lib.prelude_nenv := Some nenv;

    (* let tenv = (Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)) in *)

    (*   let globals = Closures.bindings tenv Lib.primitive_vars globals in *)
    (* (\* Debug.print ("Prelude after closure conversion: " ^ Ir.Show_program.show (globals, `Return (`Extend (StringMap.empty, None)))); *\) *)
    (*   BuildTables.bindings tenv Lib.primitive_vars globals; *)

    let valenv = Eval.run_defs Value.Env.empty globals in
    let envs =
      (valenv,
       Env.String.extend Lib.nenv nenv,
       Types.extend_typing_environment Lib.typing_env tyenv)
    in
    globals, envs
  in
  match !file_list with
  | [] -> ()
  | [src] ->
     let prelude, (_valenv, nenv, tenv) = load_prelude () in
     (* let prelude = Js.make_prelude_unit ~program:prelude ~tenv ~nenv () in *)
     (* let prelude = Jscomp.Compiler.compile_prelude prelude in *)

     let parse_and_desugar (nenv, tyenv) filename =
       let source =
         Errors.display (lazy (Loader.load_file (nenv, tyenv) filename))
       in
       let open Loader in
       let (nenv', tyenv') = source.envs in
       let nenv = Env.String.extend nenv nenv' in
       let tyenv = Types.extend_typing_environment tyenv tyenv' in
       let (globals, (locals,main), t) = source.program in
       let tenv' = Var.varify_env (nenv, tyenv.Types.var_env) in
       (* Optimise *)
       let optimise_program tenv program =
         let program = IrTraversals.ElimDeadDefs.program tenv program in
         (* let program = Ir.Inline.program tenv program in *)
         (* Printf.eprintf "Before: %s\n%!" (Ir.Show_program.show program); *)
         let program = IrTraversals.TreeShaking.program tenv program in
         (* Printf.eprintf "After: %s\n%!" (Ir.Show_program.show program); *)
         let program = IrTraversals.ElimDeadDefs.program tenv program in
         let program = IrTraversals.TidyBindings.program tenv program in
         program
       in
       (* Printf.eprintf "IR: %s\n%!" (Ir.Show_program.show (globals @ locals, main)); *)
       let program =
         if Settings.get_value BS.optimise
         then (optimise_program tenv' (prelude @ globals @ locals, main))
         else (prelude @ globals @ locals, main)
       in
       (* Printf.eprintf "Program: %s\n%!" (Ir.Show_program.show program); *)
       (* Closure convert *)
       let closure_convert tenv program =
         (* Printf.printf "Bindings:\n%s\n%!" (Ir.Show_program.show (locals, `Special (`Wrong `Not_typed))); *)
         let (globals, main) = Closures.program Lib.primitive_vars tenv program in
         BuildTables.program Lib.primitive_vars tenv' (globals, main);
         (globals, main)
       in
       let (globals, main) = closure_convert tenv' program in
       let external_files = source.external_dependencies in
       ((globals, main), t), (nenv, tyenv), external_files
     in
     (* Printf.printf "Size: %d\n" (Env.String.fold (fun _ _ acc -> acc + 1) nenv 0); *)
     let (program, _t), (nenv, tenv), alien = parse_and_desugar (nenv, tenv) src in
     let program = Js.make_comp_unit ~includes:alien ~source:src ~program ~tenv ~nenv ~target:!ParseSettings.target () in
     let program = Jscomp.Compiler.compile program in
     JsEmit.emit ~program ()

  (* let prelude, (_valenv, nenv, tenv) = load_prelude () in *)
  (* let prelude = Js.make_prelude_unit ~program:prelude ~tenv ~nenv () in *)
  (* let prelude = Jscomp.Compiler.compile_prelude prelude in *)
  (* let ast, pos_ctxt = Parse.parse_file Parse.program src in *)
  (* let (program, _t, tenv), _alien = Frontend.Pipeline.program tenv pos_ctxt ast in *)
  (* let globals, (locals, main), nenv = *)
  (*   Sugartoir.desugar_program *)
  (*     (nenv, *)
  (*      Var.varify_env (nenv, tenv.Types.var_env), *)
  (*      tenv.Types.effect_row) program *)
  (* in *)
  (* let program = Js.make_comp_unit ~source:(List.hd !file_list) ~program:(globals @ locals, main) ~tenv ~nenv ~target:!ParseSettings.target () in *)
  (*  (\* (nenv, tenv), (globals, main, t) *\) *)
  (* let program = Jscomp.Compiler.compile program in *)
  (* JsEmit.emit ~prelude ~program () *)
  | _ -> Errors.display (lazy (failwith "The JS compiler expects a single source file."))

let _ =
  if !ParseSettings.print_keywords
  then (List.iter (fun (k,_) -> print_endline k) Lexer.keywords; exit 0);

(* parse common cmdline arguments and settings *)
  begin match Utility.getenv "REQUEST_METHOD" with
    | Some _ -> Settings.set_value BS.web_mode true
    | None -> ()
  end;

  if Settings.get_value BS.Js.compile
  then compile_js ()
  else main()

