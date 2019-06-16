(* Compilation environment. *)
open Utility

(* module type INTERFACE = sig
 *   type t
 * 
 *   val empty : t
 *   val from_environments : Types.typing_environment -> Var.var Env.String.t -> t
 *   val to_environments : t -> Types.typing_environment * Var.var Env.String.t
 * 
 *   module Typedefs: sig
 *     val mem : string -> t -> bool
 *     val add : string -> Types.tycon_spec -> t -> t
 *     val find : string -> t -> Types.tycon_spec
 *   end
 * 
 *   module Vars: sig
 *     val mem : string -> t -> bool
 *     val type_of : string -> t -> Types.datatype
 *     val var_of : string -> t -> Var.var
 * 
 *     val add : string -> Var.var -> t -> t
 *   end
 * 
 *   (\* Computes the union of two signatures. In the event of a name
 *      clash the second argument takes precedence. *\)
 *   val union : t -> t -> t
 * end
 * 
 * module Interface : INTERFACE = struct
 *   (\* TODO a compilation unit / module signature should only expose its
 *      public interface. The typing environment contains too much
 *      information. *\)
 *   type t =
 *     { tyenv: Types.typing_environment;
 *       varenv: Var.var Env.String.t }
 * 
 *   let empty =
 *     { varenv = Env.String.empty;
 *       tyenv = Types.empty_typing_environment }
 * 
 *   let from_environments tyenv varenv =
 *     { tyenv; varenv }
 * 
 *   let to_environments { tyenv; varenv } = tyenv, varenv
 * 
 *   open Types
 *   module Typedefs = struct
 *     let mem name { tyenv; _ } =
 *       Env.String.mem tyenv.tycon_env name
 * 
 *     let add name tycon env =
 *       { env with tyenv = { env.tyenv with tycon_env = Env.String.bind env.tyenv (name, tycon) } }
 * 
 *     let find name env =
 *       Env.String.lookup env.tyenv.tycon_env name
 *   end
 * 
 *   module Vars = struct
 *     let mem name { tyenv; _ } =
 *       Env.String.(has tyenv.var_env name || has tyenv.tycon_env name)
 *     let type_of name { tyenv; _ } = Env.String.lookup tyenv.var_env name
 *     let var_of name { varenv; _ } = Env.String.lookup varenv name
 * 
 *     let add name var datatype { tyenv; varenv } =
 *       let open Types in
 *       { tyenv = { tyenv.var_env with var_env = Env.String.bind tyenv.var_env (name, datatype) };
 *         varenv = Env.String.bind varenv (name, var) }
 *   end
 * 
 *   let union s1 s2 =
 *     { tyenv = Types.extend_typing_environment s1.tyenv s2.tyenv;
 *       varenv = Env.String.extend s1.varenv s2.varenv }
 * end *)


module CompUnit = struct
  type source =
    [ `Empty
    | `SugarBindings of Sugartypes.binding list
    | `SugarProgram of Sugartypes.program
    | `Ir of Ir.program ]
  and source' = Abstract
              | Concrete of { filename: string; source: source }
  (* The following is somewhat coarse; it would be nice to a an
     "interface" abstraction, which only keeps track of the public
     members, but since everything is public at the moment it doesn't
     really matter much. *)
  and data = { valenv: Value.env;
               tyenv: Types.typing_environment;
               varenv: Var.var Env.String.t;
               source: source' }

  type t =
    { data: data;
      ident: int;
      name: string }

  let empty_data is_abstract filename =
    { valenv = Value.Env.empty;
      tyenv = Types.empty_typing_environment;
      varenv = Env.String.empty;
      source = if is_abstract then Abstract
               else Concrete { filename; source = `Empty } }
  let make =
    let id = ref 0 in
    fun data name ->
    incr id;
    { ident = !id; name; data }

  let make_abstract name = make (empty_data true "") name
  let make_concrete filename name = make (empty_data false filename) name

  let is_loaded { data; _ } =
    match data.source with
    | Concrete { source = `Empty; _ } -> false
    | _ -> true

  let is_abstract { source; _ } =
    match source with
    | Abstract -> true
    | _ -> false

  module Source = struct
    let promote : source -> t -> t
      = fun source cunit ->
      let source = match source, cunit.data.source with
        | `SugarBindings _, Concrete ({ source = `Empty; _ } as r)
        | `SugarProgram _, Concrete ({ source = `Empty; _ } as r)
        | `SugarProgram _, Concrete ({ source = `SugarBindings _; _ } as r)
        | `Ir _, Concrete r -> Concrete { r with source }
        | _, Abstract -> raise (Invalid_argument "Cannot promote an abstract compilation unit.")
        | _, _ -> raise (Invalid_argument "Promotion error.")
      in
      { cunit with data = { cunit.data with source } }

    (* Low-level access to the environments. Usage should be
       restricted as these are highly to be replaced in favour of
       something more structured. *)
    module Valenv = struct
      let set : Value.env -> t -> t
        = fun valenv cunit ->
        { cunit with data = { cunit.data with valenv } }

      let get : t -> Value.env
        = fun cunit -> cunit.data.valenv
    end

    module Varenv = struct
      let set : Var.var Env.String.t -> t -> t
        = fun varenv cunit ->
        { cunit with data = { cunit.data with varenv } }

      let get : t -> Var.var Env.String.t
        = fun cunit -> cunit.data.varenv
    end

    module Typenv = struct
      let set : Types.typing_environment -> t -> t
        = fun tyenv cunit ->
        { cunit with data = { cunit.data with tyenv } }

      let get : t -> Types.typing_environment
        = fun cunit -> cunit.data.tyenv
    end
  end

  let filename cunit = match cunit.data.source with
    | Concrete { filename; _ } -> filename
    | Abstract -> raise (Invalid_argument "An abstract compilation has no corresponding file.")

  let name cunit = cunit.name
end

module CompEnv = struct
  type t =
    { units: CompUnit.t StringMap.t;
      predef: CompUnit.t StringMap.t (* May be shadowed by [units]. *) }

  module Predef = struct
    let mem : string -> t -> bool
      = fun name { predef; _ } -> StringMap.mem name predef
    let find : string -> t -> CompUnit.t
      = fun name { predef; _ } -> StringMap.find name predef
    let add : string -> CompUnit.t -> t -> t
      = fun name cunit ({ predef; _ } as e) ->
      { e with predef = StringMap.add name cunit predef }

    let is_prelude_loaded : t -> bool = mem "Prelude"
    let prelude : t -> CompUnit.t = find "Prelude"
    let is_lib_loaded : t -> bool = mem "Lib"
    let lib : t -> CompUnit.t = find "Lib"
  end

  let find : string -> t -> CompUnit.t
    = fun name e ->
    try StringMap.find name e.units
    with Notfound.NotFound _ -> Predef.find name e

  let mem : string -> t -> bool
    = fun name e ->
    StringMap.mem name e.units || Predef.mem name e

  let add : string -> CompUnit.t -> t -> t
    = fun name cunit env ->
    { env with units = StringMap.add name cunit env.units }

end
include CompEnv
