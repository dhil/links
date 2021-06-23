(** {0 IR variables} *)

open Utility

module Scope = Binder.Scope

(** Term variables *)
type var = Binder.Var.t
  [@@deriving show,eq,yojson]
type var_info = Binder.Info.t
  [@@deriving show]
type binder = Binder.t
  [@@deriving show]

let dummy_var = 0

(** Generate just a fresh identifier *)
let fresh_raw_var : unit -> var = Binder.Var.next

(** Given metadata, generate a full binder *)
let fresh_binder : var_info -> binder =
  fun info -> Binder.of_info info (fresh_raw_var ())

(** Given metadata, generate a full binder and pair it with the new
    variable identifer; note this identifier is already the first
    component of the [binder] value *)
let fresh_var : var_info -> binder * var
  = fun info ->
    let b = fresh_binder info in
    b, Binder.var b

(** {0 Manipulate binder metadata} *)

let info_type _info = failwith "var.ml: info_type deprecated"
let info_of_type t = Binder.Info.make Scope.Local t ""

let make_info t name scope = Binder.Info.make scope t name
let make_local_info  (t, name) = make_info t name Scope.Local
let make_global_info (t, name) = make_info t name Scope.Global

let make_binder var info = Binder.of_info info var
let update_type newtype b = Binder.set_datatype b

let fresh_binder_of_type = info_of_type ->- fresh_binder
let fresh_var_of_type = info_of_type ->- fresh_var
let fresh_global_var_of_type = info_of_type ->- fresh_var

let var_of_binder b = Binder.var b
let info_of_binder b = Binder.info b
let type_of_binder b = Binder.datatype b
let name_of_binder b = Binder.name b
let scope_of_binder b = if Binder.is_global b
                        then Scope.Global
                        else Scope.Local

let globalise_binder b = Binder.globalise b

(** Create a copy of a type environment mapping vars (= ints) to types
    instead of strings to types
*)
let varify_env (nenv, tenv) : Types.datatype Env.Int.t =
  Env.String.fold
    (fun name t tenv ->
       Env.Int.bind (Env.Name.find name nenv) t tenv)
    tenv
    Env.Int.empty
