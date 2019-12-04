include Transform.Typeable.S
include Transform.Untyped.S

module PatchBuiltinsTypeSignatures: sig
  val program : Sugartypes.program -> Sugartypes.program
end
