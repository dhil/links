module type EMITTER = sig
  type 'a t
  val gen : Js.program Js.comp_unit -> 'a t
  val run : 'a t -> unit
  val emit_comment : 'a t -> string -> unit
  val emit_return : 'a t -> unit

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pure : 'a -> 'a t
end

module JsEmitter = struct
  type 'a t = Buffer.t

  let pure _ = Buffer.create 256

  let gen u = pure u
end

let emit : program:Js.program Js.comp_unit -> unit -> unit
  = fun ~program () -> ()
