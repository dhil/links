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


module type WHOLE_PROGRAM_ASSEMBLER = sig
  val assemble : unit -> unit
end

(* Interface description for loaders. *)
module type LOADER = sig
  type t

  val make : unit -> t
  val load_file : t -> string -> t
  val bootstrap : ?prelude:string ->
                  ?virtual_units:(Var.var Env.String.t * Types.typing_environment) list -> (* TODO introduce a structured abstraction for "virtual" compilation units. *)
                  ?units:string list -> t -> t
end

module type LEGACY_LOADER = sig
  include LOADER
  module Monolithic_program: sig
    type t =
      { varenv: Var.var Env.String.t;
        tyenv: Types.typing_environment;
        program: Ir.program * Types.datatype;
        aliens: string list }
  end

  val load_file : t -> string -> Monolithic_program.t * t
end

module Legacy : LEGACY_LOADER = struct

  type t =
    { varenv: Var.var Env.String.t;
      tyenv: Types.typing_environment;
      preamble: Ir.program * Types.datatype;
      aliens: string list }

  module Monolithic_program = struct
    type t =
      { varenv: Var.var Env.String.t;
        tyenv: Types.typing_environment;
        program: Ir.program * Types.datatype;
        aliens: string list }
  end

  let empty_program : Ir.program
    = ([], Ir.(Return (Extend (StringMap.empty, None))))

  let make () =
    let varenv = Env.String.empty in
    let tyenv = Types.empty_typing_environment in
    { varenv; tyenv; aliens = [];
      preamble = (empty_program, Types.unit_type) }

  let load : string -> t -> t
    = fun filename st ->
    let program, pos_context =
      ModuleUtils.try_parse_file filename in
    let whole_program = Chaser.add_dependencies program in
    let ((whole_program, t, tyenv), aliens) =
      Frontend.Pipeline.program st.tyenv pos_context whole_program
    in
    let whole_program, varenv =
      let open Types in
      let globals, main, varenv =
        Sugartoir.desugar_program
          ( st.varenv
          , Var.varify_env (st.varenv, tyenv.var_env)
          , tyenv.effect_row)
          whole_program
      in
      Ir.with_bindings globals main, varenv
    in
    let aliens =
      ListUtils.unduplicate String.equal (st.aliens @ aliens)
    in
    let whole_program =
      if (fst st.preamble) == empty_program (* pointer equality *)
      then whole_program
      else let (bs, tc), ty = st.preamble in
           let b =
             let info = Var.make_global_info (ty, "synthetic_") in
             let bndr = Var.fresh_binder info in
             Ir.Let (bndr, ([], tc))
           in
           Ir.with_bindings (bs @ [b]) whole_program
    in
    let varenv = Env.String.extend st.varenv varenv in
    let tyenv = Types.extend_typing_environment st.tyenv tyenv in
    { varenv; tyenv; aliens; preamble = (whole_program, t) }

  (** Read source code from a file, parse, infer types and desugar to
      the IR *)
  let load_file st filename =
    let st' = load filename st in
    let mono_prog =
      let varenv, tyenv, aliens = st'.varenv, st'.tyenv, st'.aliens in
      let open Monolithic_program in
      { varenv; tyenv; aliens;
        program = st'.preamble }
    in
    mono_prog, st (* intentionally returns the old (bootstrapped) loader state. *)

  let bootstrap ?prelude ?(virtual_units=[]) ?(units=[]) st =
    let st =
      List.fold_left
        (fun ({ varenv; tyenv; _ } as st) (varenv', tyenv') ->
          let varenv = Env.String.extend varenv varenv' in
          let tyenv = Types.extend_typing_environment tyenv tyenv' in
          { st with varenv; tyenv })
        st virtual_units
    in
    let st =
      match prelude with
      | None -> st
      | Some filename ->
         let st = load filename st in
         let st = { st with tyenv = Lib.patch_prelude_funs st.tyenv } in
         Lib.prelude_tyenv := Some st.tyenv;
         Lib.prelude_nenv := Some st.varenv; st
    in
    List.fold_left (flip load) st units
end




