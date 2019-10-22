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

  module Ident: sig
    type comp_unit = t
    module Binder: sig
      module Scope: sig
        type t = Local | Global
                 [@@deriving show]
        val is_global : t -> bool
        val is_local : t -> bool
      end

      type t [@@deriving show]

      val fresh : ?datatype:Types.datatype -> ?scope:Scope.t -> comp_unit -> string -> t
      val equal : t -> t -> bool
      val compare : t -> t -> int
    end

    (* Compilation unit names, interface names. *)
    module Persistent: sig
      include module type of Pident
    end

    (* Compilation unit local names. *)
    module Local: sig
      type t [@@deriving show]

      val make : Binder.t list -> t
      val equal : t -> t -> bool
      val compare : t -> t -> int
    end

    (* Compilation unit remote names. *)
    module Remote: sig
      type t [@@deriving show]

      val make : comp_unit -> Persistent.t list -> t
      val equal : t -> t -> bool
      val compare : t -> t -> int
    end
  end

  val of_filename : string -> t
  val interactive : string -> t

  val name : t -> string
  val identifier : t -> Ident.Persistent.t

  val depend : t -> t -> t
  val depends : t -> t -> bool

  val name_of_filename : string -> string
end

                    (* Compilation environment. *)
module Comp_env: sig
  type t [@@deriving show]
end
