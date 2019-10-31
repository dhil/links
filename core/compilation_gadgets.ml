(* Compilation environment. *)
open Utility
open Ident

(* Mapping interface names to implementation-specific names. *)
module Implementation = struct
  type member =
    { var: Local.t;
      kind: Types.Interface.member }
  type t = member Persistent.Map.t

  let empty = Persistent.Map.empty

  let find ident impl =
    Persistent.Map.find ident impl

  let size impl =
    Persistent.Map.size impl
end

module Comp_unit = struct
  type kind = Physical of string (* filename. *)
            | Virtual of string (* compilation unit name. *)
  type t =
    { kind: kind;
      gensym: Gensym.t;
      ident: Persistent.t;
      dependencies: Persistent.Set.t;
      interface: Types.Interface.t;
      implementation: Implementation.t }

  let name_of_filename filename =
    String.capitalize_ascii
      Filename.(remove_extension (basename filename))

  let name { kind; _ } =
    match kind with
    | Physical filename -> name_of_filename filename
    | Virtual name  -> name

  let identifier : t -> Persistent.t
    = fun c -> c.ident

  let make kind name =
    { kind;
      gensym = Gensym.make ();
      dependencies = Persistent.Set.empty;
      ident = Persistent.of_string name;
      interface = Types.Interface.empty;
      implementation = Implementation.empty }

  let of_filename filename =
    make (Physical filename) (name_of_filename filename)

  let interactive name =
    make (Virtual name) name

  let depend : t -> t -> t
    = fun dependee dependant ->
    let dependencies' =
      Ident.Persistent.Set.add (identifier dependee) dependant.dependencies
    in
    { dependant with dependencies = dependencies' }

  let depend_many : t list -> t -> t
    = List.fold_right depend

  let depends dependee dependant =
    Ident.Persistent.Set.mem (identifier dependee) dependant.dependencies

  let interface { interface; _ } = interface
  let implementation { implementation; _ } = implementation

  let is_physical { kind; _ } =
    match kind with
    | Physical _ -> true
    | _ -> false

  let is_virtual c = not (is_physical c)

  module Gensym = struct
    let next { gensym; _ } =
      Gensym.next gensym
  end

  module Binder = struct
    type comp_unit = t
    include Binder

    let fresh : ?datatype:Types.datatype -> ?scope:Scope.t -> comp_unit -> string -> t
      = fun ?(datatype=`Not_typed) ?(scope=Scope.Local) comp_unit name ->
      let ident = Ident.fresh comp_unit.gensym in
      make ~datatype ~scope (identifier comp_unit) ident name
  end

  module Implementation = Implementation
end

module Comp_env = struct
  type t = unit
         [@@deriving show]
end
