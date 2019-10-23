(* This module contains the following gadgets:
    - Compilation unit
    - Compilation environment
 *)

(* Compilation units. *)
module Comp_unit: sig
  type t

  module Gensym: sig
    val next : t -> int
  end

  module Binder: sig
    type comp_unit = t
    include Ident.Binder.S with type t = Ident.Binder.t
    val fresh : ?datatype:Types.datatype -> ?scope:Scope.t -> comp_unit -> string -> t
  end

  (* Mapping interface names to implementation-specific names. *)
  module Implementation: sig
    type member =
      { var: Ident.Local.t;
        kind: Types.Interface.member }
    type t

    val empty : t
    val find : Pident.t -> t -> member
    val size : t -> int
  end

  val of_filename : string -> t
  val interactive : string -> t

  val name : t -> string
  val identifier : t -> Ident.Persistent.t

  val depend : t -> t -> t
  val depends : t -> t -> bool
  val depend_many : t list -> t -> t

  val interface : t -> Types.Interface.t
  val implementation : t -> Implementation.t

  val name_of_filename : string -> string
end

                    (* Compilation environment. *)
module Comp_env: sig
  type t [@@deriving show]
end
