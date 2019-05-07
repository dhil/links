open Utility

type envs = Var.var Env.String.t * Types.typing_environment
type program = Ir.binding list * Ir.computation * Types.datatype

(* Filename of an external dependency *)
type ext_dep = string

(* Result of loading a file *)
type source = {
  envs: envs;
  program: program;
  external_dependencies: ext_dep list
}

(** Read source code from a file, parse, infer types and desugar to
    the IR *)
let load_file (nenv, tyenv) (filename:string) =
  let module Parser = struct
      let parse file = Parse.parse_file Parse.program file
    end
  in
  let path =
    match String.split_on_char ':' (Settings.get_value Basicsettings.links_file_paths) with
    | [""] -> []
    | paths -> paths
  in
  let module Preloader = Preloader.Make(Parser) in
  (* Bootstrapping *)
  let loader = Preloader.make ~path () in
  let _, loader = Preloader.bootstrap "lib.links" loader in
  let prelude, loader = Preloader.bootstrap (Settings.get_value Basicsettings.prelude_file) loader in
  (* TODO patch prelude. *)
  let _ =
    Printf.printf "=== Loader state after bootstrapping %s\n%!" filename;
    Preloader.dump stderr loader;
  in
  (* After bootstrapping *)
  let open Preloader in
  let _ =
    let _, loader = Preloader.preload ~implicit_dependencies:[prelude] filename loader in
    Printf.printf "=== Loader state after input %s\n%!" filename;
    Preloader.dump stderr loader;
    let lo = Preloader.compute_load_order loader in
    LoadOrder.load (fun comp_unit store _ ->
        let open Compilation_unit in
        let qname = Store.ById.as_qualified_name comp_unit.id store in
        Printf.printf "(%d) %s ->\n%!" comp_unit.id (String.concat "." qname))
      lo ()
  in
  let sugar, pos_context =
    ModuleUtils.try_parse_file filename in
  (* printf "AST: \n %s \n" (Sugartypes.show_program sugar); *)
  let ((program, t, tenv), ffi_files) = Frontend.Pipeline.program tyenv pos_context sugar in
  let globals, main, nenv =
    Sugartoir.desugar_program
      (nenv,
       Var.varify_env (nenv, tyenv.Types.var_env),
       tyenv.Types.effect_row) program
  in
  {
    envs = (nenv, tenv);
    program = (globals, main, t);
    external_dependencies = ffi_files
  }




