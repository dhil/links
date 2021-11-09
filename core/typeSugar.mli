module Check :
  sig
    open Sugartypes

    val program : Compenv.t
               -> Types.typing_environment
               -> program
               -> program * Types.datatype * Types.typing_environment

    val sentence : Compenv.t
                -> Types.typing_environment
                -> sentence
                -> sentence * Types.datatype * Types.typing_environment
  end
