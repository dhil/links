module Env = struct
  type 'a t = 'a Env.Ident.t

  let empty = Env.Ident.empty

  let bind v t env = Env.Ident.bind env (v, t)

  let unbind v env = Env.Ident.unbind env v

  let singleton v t = bind v t empty

  let lookup v ctxt = Env.Ident.lookup ctxt v

  let extend env env' = Env.Ident.extend env env'

  let domain env = Env.Ident.domain env
  let range env = Env.Ident.range env

  let map f env = Env.Ident.map f env

  module Dom = Env.Ident.Dom
end
type t =
  { (* Compilation state. *)
    comp_state: Context.t;
    (* Mapping variables to types. *)
    var_env: Types.datatype Env.t;
    (* Variables which are recursive in the current scope. *)
    rec_vars: Ident.Set.t;
    (* Mapping (compilation unit local) type alias names to their
       definitions. *)
    tycon_env: Types.tycon_spec Env.t;
    (* The current effects. *)
    effect_row: Types.row;
    (* Whether to permit occurrences of non-user facing constructs
       introduced after desugaring. *)
    desugared: bool }

let make ?(eff=Types.make_empty_open_row default_effect_subkind) ?(desugared=false) context =
  { comp_state = context;
    var_env = Env.empty;
    rec_vars  = Ident.Set.empty;
    tycon_env = Env.empty;
    effect_row = eff;
    desugared = desugared }

let ambient { comp_state; _ } = comp_state

let effects { effect_row; _ } = effect_row

let with_effects effect_row ctxt =
  { ctxt with effect_row }

let add_rec_var ident ctxt =
  { ctxt with rec_vars = Ident.Set.add ident ctxt.rec_vars }

let desugared { desugared; _ } = desugared

let bind v t ctxt =
  { ctxt with var_env = Env.bind v t ctxt.var_env }

let unbind v ctxt =
  { ctxt with var_env = Env.unbind v ctxt.var_env }

let lookup : Name.t -> t -> Types.datatype = function
  | Name.Unresolved _ -> assert false
  | Name.Local v -> assert false
  | Name.Remote v -> assert false

let modify ?var_env ?effect_row ?rec_vars ?tycon_env context =
  let context' =
    match var_env with
    | None -> context
    | Some var_env -> { context with var_env }
  in
  let context'' =
    match effect_row with
    | None -> context'
    | Some effect_row -> { context with effect_row }
  in
  let context''' =
    match rec_vars with
    | None -> context''
    | Some rec_vars -> { context with rec_vars }
  in
  match tycon_env with
  | None -> context'''
  | Some tycon_env -> { context with tycon_env }

let variable_environment { var_env; _ } = var_env
