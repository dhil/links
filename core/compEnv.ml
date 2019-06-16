(* Compilation environment. *)
open Utility

module type INTERFACE = sig
  type t

  val empty : t
  val from_environments : Types.typing_environment -> Var.var Env.String.t -> t
  val to_environments : t -> Types.typing_environment * Var.var Env.String.t

  val mem : string -> t -> bool
  val type_of : string -> t -> Types.datatype
  val var_of : string -> t -> Var.var

  (* Computes the union of two signatures. In the event of a name
     clash the second argument takes precedence. *)
  val union : t -> t -> t
end

module Interface : INTERFACE = struct
  (* TODO a compilation unit / module signature should only expose its
     public interface. The typing environment contains too much
     information. *)
  type t =
    { tyenv: Types.typing_environment;
      varenv: Var.var Env.String.t }

  let empty =
    { varenv = Env.String.empty;
      tyenv = Types.empty_typing_environment }

  let from_environments tyenv varenv =
    { tyenv; varenv }

  let to_environments { tyenv; varenv } = tyenv, varenv

  open Types
  let mem name { tyenv; _ } =
    Env.String.(has tyenv.var_env name || has tyenv.tycon_env name)
  let type_of name { tyenv; _ } = Env.String.lookup tyenv.var_env name
  let var_of name { varenv; _ } = Env.String.lookup varenv name

  let union s1 s2 =
    { tyenv = Types.extend_typing_environment s1.tyenv s2.tyenv;
      varenv = Env.String.extend s1.varenv s2.varenv }
end


module CompUnit = struct
  type source =
    [ `SugarBindings of Sugartypes.binding list
    | `SugarProgram of Sugartypes.program
    | `Ir of Ir.program ]
  type source' =
    [ `Empty
    | source ]

  type descriptor =
    { filename: string }

  type t =
    { source: source';
      ident: int;
      name: string;
      interface: Interface.t;
      provenance: descriptor }


  let empty =
    let id = ref 0 in
    fun () ->
    incr id;
    { source = Empty;
      ident = !id;
      name = "";
      interface = interface.empty;
      provenance = { filename = "" } }

  let is_loaded { source; _ } =
    match source with
    | `Empty -> false
    | _ -> true

  let promote : source -> t -> t
    = fun source cunit ->
    let source = match source, cunit.source with
      | `SugarBindings _, `Empty -> source
      | `SugarProgram _, `Empty
      | `SugarProgram _, `SugarBindings _ -> source
      | `Ir _, _ -> source
      | _, _ -> failwith "promotion error."
    in
    { cunit with source }

  let set_filename filename cunit =
    { cunit with provenance = { filename } }
  let filename cunit = cunit.provenance.filename

  let name cunit = cunit.name
  let set_name name cunit = { cunit with name }

  let equal : t -> t -> bool
    = fun cunit' cunit ->
    cunit.ident = cunit'.ident && cunit.source == cunit'.source

  let set_interface interface cunit = { cunit with interface }
  let interface cunit = cunit.interface
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

end
include CompEnv
