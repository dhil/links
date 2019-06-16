(* An intermediate abstraction between the frontend compiler and
   backend compiler. *)

module type Comp_State_Ops = sig
  type state
  type target
  val get : state -> target
  val set : target -> state -> state
end

module type COMPILATION_STATE = sig
  type t

  val initialise : ?valenv:Value.env ->
                   ?varenv:Var.var Env.String.t ->
                   ?tyenv:Types.typing_environment ->
                   ?aliens:string list -> unit -> t

  module Values : Comp_State_Ops
         with type state := t
         and type target := Value.env

  module Aliens : Comp_State_Ops
         with type state := t
         and type target := string list

  module Types : Comp_State_Ops
         with type state := t
         and type target := Types.typing_environment

  module Vars : Comp_State_Ops
         with type state := t
         and type target := Var.var Env.String.t
end

module State : COMPILATION_STATE = struct
  type t =
    { valenv: Value.env;
      varenv: Var.var Env.String.t;
      tyenv: Types.typing_environment;
      aliens: string list }

  let initialise : ?valenv:Value.env ->
                   ?varenv:Var.var Env.String.t ->
                   ?tyenv:Types.typing_environment ->
                   ?aliens:string list -> unit -> t
    = fun ?(valenv=Value.Env.empty)
          ?(varenv=Env.String.empty)
          ?(tyenv=Types.empty_typing_environment)
          ?(aliens=[]) () ->
    { valenv; varenv; tyenv; aliens }

  module Aliens = struct
    let get { aliens; _ } = aliens
    let set aliens st = { st with aliens }
  end

  module Values = struct
    let get { valenv; _ } = valenv
    let set valenv st = { st with valenv }
  end

  module Vars = struct
    let get { varenv; _ } = varenv
    let set varenv st = { st with varenv }
  end

  module Types = struct
    let get { tyenv; _ } = tyenv
    let set tyenv st = { st with tyenv }
  end
end

type result =
  { value: Value.t;
    datatype: Types.datatype }

module Evaluate = struct
  let eval : State.t -> Ir.program -> result * State.t
    = fun st prog -> assert false
end

module WebRuntime = struct
  let eval : State.t -> Ir.program ->  result * State.t
    = fun st prog -> assert false
end

module REPL = struct
  let boot : unit -> unit
    = fun () -> ()
end

let program : State.t -> Ir.program -> Ir.program * State.t
  = fun st program ->
  let open Types in
  let tyenv = State.Types.get st in
  let tenv = Var.varify_env (State.Vars.get st, tyenv.var_env) in
  let module BS = Basicsettings in
  let perform_optimisations = Settings.get_value BS.optimise && not (Settings.get_value BS.interactive_mode) in
  let program =
    Backend.transform_program perform_optimisations tenv program
  in
  assert false
