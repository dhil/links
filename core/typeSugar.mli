module Check :
  sig
    open Sugartypes

    val program : Context.t
               -> program
               -> program * Types.datatype * Context.t

    val sentence : Context.t
                -> sentence
                -> sentence * Types.datatype * Context.t
  end
