(* Compilation environment. *)
open Utility
open Ident

(* Mapping interface names to implementation-specific names. *)
module Implementation = struct
  type member =
    { var: Local.t;
      kind: Types.Interface.member }
  type t = member Pident.Map.t

  let empty = Pident.Map.empty

  let find ident impl =
    Pident.Map.find ident impl

  let size impl =
    Pident.Map.size impl
end

module Comp_unit = struct
  type kind = Physical of string (* filename. *)
            | InMemory of string (* compilation unit name. *)
  type t =
    { kind: kind;
      mutable next: int;
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
    | InMemory name  -> name

  let identifier : t -> Persistent.t
    = fun c -> c.ident

  let make kind name =
    { kind; next = 1;
      dependencies = Pident.Set.empty;
      ident = Pident.of_name name;
      interface = Types.Interface.empty;
      implementation = Implementation.empty }

  let of_filename filename =
    make (Physical filename) (name_of_filename filename)

  let interactive name =
    make (InMemory name) name

  let depend : t -> t -> t
    = fun dependee dependant ->
    let dependencies' =
      Pident.Set.add (identifier dependee) dependant.dependencies
    in
    { dependant with dependencies = dependencies' }

  let depend_many : t list -> t -> t
    = List.fold_right depend

  let depends dependee dependant =
    Pident.Set.mem (identifier dependee) dependant.dependencies

  let interface { interface; _ } = interface
  let implementation { implementation; _ } = implementation

  module Gensym = struct
    let next c =
      let n = c.next in
      c.next <- n + 1; n
  end

  module Binder = struct
    type comp_unit = t
    include Ident.Binder

    let fresh : ?datatype:Types.datatype -> ?scope:Scope.t -> comp_unit -> string -> t
      = fun ?(datatype=`Not_typed) ?(scope=Scope.Local) c name ->
      let ident = Gensym.next c in
      make ~datatype ~scope (identifier c) ident name
  end

  module Implementation = Implementation
end

module Comp_env = struct
  type t = unit
         [@@deriving show]
end
