(** Converts the tree returned by the parser into our internal
    representation *)

val show_compiled_ir : bool Settings.setting

type result =
  { globals: Ir.binding list;
    program: Ir.program;
    datatype: Types.datatype;
    context: Context.t }

val program : Context.t -> Types.datatype -> Sugartypes.program -> result
