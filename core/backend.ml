open Utility

 (* TODO: optimisation *)

  (* We need to be careful here as, for instance, running ElimDeadDefs
     on the prelude would lead to lots of functions being deleted that
     might actually be used in the program itself.

     It does cause problems in the interactive loop
     as if you define a function it immediately gets optimised away as
     it isn't yet used!

     In order to resolve the problem we could either simply disable
     optimisation for the interactive loop, or we could more usefully
     disable optimisation of the top level definitions by returning a
     tuple containing all of them. That way we could also optimise the
     prelude.
  *)


let guard predicate transformer =
              if predicate then transformer else (fun _ x -> x)
let guarded setting =
  guard (Settings.get_value setting)


let print_program _ p =
  Debug.print (Ir.string_of_program p);p

let print_bindings _ bs =
  List.iter (Debug.print -<- Ir.string_of_binding) bs;bs


let run pipeline tyenv p =
  Array.fold_left (fun p transformer -> transformer tyenv p) p pipeline

let measure name func tyenv p = Performance.measure name (uncurry func) (tyenv, p)

let mutate transformer tyenv p =
  ignore (transformer tyenv p); p

module Pipeline = struct

  module Program = struct
    let optimisation =
      [| IrTraversals.ElimDeadDefs.program;
         IrTraversals.Inline.program |]

    let type_check =
      [|(*IrTraversals.NormaliseTypes.program;
         IrTraversals.ElimRecursiveTypeCycles.program;*)
         IrTraversals.ElimTypeAliases.program;
         IrTraversals.ElimBodiesFromMetaTypeVars.program;
         (guarded Basicsettings.Ir.show_compiled_ir_after_backend_transformations print_program);
         IrCheck.Typecheck.program |]

    let main ~optimise () =
      [| guard optimise (measure "optimise" (run optimisation));
         Closures.program Lib.primitive_vars;
         mutate (BuildTables.program Lib.primitive_vars);
         guarded Basicsettings.Ir.typecheck_ir (run type_check) |]
  end

  module Bindings = struct
    let type_check =
      [| (*IrTraversals.NormaliseTypes.program;
          IrTraversals.ElimRecursiveTypeCycles.program;*)
         IrTraversals.ElimTypeAliases.bindings;
         IrTraversals.ElimBodiesFromMetaTypeVars.bindings;
         (guarded
            Basicsettings.Ir.show_compiled_ir_after_backend_transformations
            print_bindings);
         IrCheck.Typecheck.bindings |]

    let main =
      [| (* May perform some optimisations here that are safe to do on the prelude *)
         (fun tenv globals -> Closures.bindings tenv Lib.primitive_vars globals);
         (fun tenv globals -> BuildTables.bindings tenv Lib.primitive_vars globals; globals);
         guarded Basicsettings.Ir.typecheck_ir (run type_check) |]
  end

end


let program ?(optimise=false) tyenv program =
  run (Pipeline.Program.main ~optimise ()) tyenv program

let bindings tyenv bindings =
  run Pipeline.Bindings.main tyenv bindings
