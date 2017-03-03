(** Continuation signatures -- the terminology is adopted from our
    paper. **)

module type PURECONTINUATION = sig
  (* Continuation frame *)
  module Frame : sig
    type nonrec t
    val make : Value.env -> Ir.scope -> Ir.var -> Ir.computation -> t
  end

  (* The result type of continuation application *)
  type r
    
  (* Continuations form a monoid *)    
  type t
  val empty : t
  val (<>) : t -> t -> t

  (* Augmentation operator -- augments a given continuation by one frame *)
  val (&>) : Frame.t -> t -> t

  (* Applies the continuation [cont] with argument [arg] in context [env] *)
  val apply : env:Value.env -> cont:t -> arg:Value.t -> r

  (* Tests whether a given continuation is empty *)
  val exhausted : t -> bool
end
  
module type CONTINUATION = sig
  include PURECONTINUATION
    
  type clauses = unit      (* dummy type *)
  type handler_spec = unit (* dummy type *)
    
  val install_handler : Value.env * clauses * handler_spec -> t -> t
  val handle : t -> op:(Ir.name * Ir.value) -> r
end
