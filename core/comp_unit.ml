(* Compilation unit structure *)
module Struct = struct
  type t = Physical of { fname: string }
         | Synthetic of { name: string }
end

type t = { iface: Types.Interface.t
         ; structure: Struct.t }

module Interface = struct
  let lookup_type name { iface; _ } = Types.Interface.lookup_type name iface
  let lookup_type' cname { iface; _ } = Types.Interface.lookup_type' cname iface

  let extend ext ({ iface; _ } as cu) =
    let iface' =
      Types.Interface.fold
        (fun name (cname, ty) iface ->
          Types.Interface.extend name cname ty iface)
        ext iface
    in
    { cu with iface = iface' }

  let replace iface cu = { cu with iface }
end

module Implementation = struct
  let canonical_name name { iface; _ } = Types.Interface.canonical_name name iface
end

module Make = struct
  let physical fname = { iface = Types.Interface.empty
                       ; structure = Struct.Physical { fname } }
  let synthetic name = { iface = Types.Interface.empty
                       ; structure = Struct.Synthetic { name } }
  let interactive () = synthetic "<interactive>"
end

let is_physical { structure; _ } = match structure with Struct.Physical _ -> true | _ -> false
let is_synthetic cu = not (is_physical cu)
let is_interactive { structure; _ } = match structure with Struct.Synthetic { name = "<interactive>" } -> true | _ -> false

let name { structure; _ } = match structure with
  | Struct.Physical { fname } -> fname
  | Struct.Synthetic { name } -> name
