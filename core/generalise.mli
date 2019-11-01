open CommonTypes

val generalise : ?unwrap:bool -> TypingContext.t -> Types.datatype
              -> ((Quantifier.t list * Types.type_arg list) * Types.datatype)
val generalise_rigid : ?unwrap:bool -> TypingContext.t -> Types.datatype
                    -> ((Quantifier.t list * Types.type_arg list) * Types.datatype)
val get_quantifiers_rigid : TypingContext.t -> Types.datatype -> Quantifier.t list
val rigidify_type_arg : Types.type_arg -> unit
