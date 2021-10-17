(* Compilation environment *)
open Utility

type t = { lib: Comp_unit.t
         ; prelude: Comp_unit.t
         ; sources: Comp_unit.t StringMap.t }

let empty = { lib = Comp_unit.Make.synthetic "lib"
            ; prelude = Comp_unit.Make.synthetic "prelude"
            ; sources = StringMap.empty }

let set_prelude prelude_cu cenv = { cenv with prelude = prelude_cu }
let prelude { prelude; _ } = prelude

let set_lib lib_cu cenv = { cenv with lib = lib_cu }
let lib { lib; _ } = lib

let bind name cu ({ sources; _ } as cenv) = { cenv with sources = StringMap.add name cu sources }
let find name { sources; _ } = StringMap.lookup name sources
