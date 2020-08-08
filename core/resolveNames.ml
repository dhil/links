module NameEnv = struct
  open Utility

  module StringMap = struct
    include StringMap

    (* Extends [m0] by [m1]. In case [m0] and [m1] overlap then the
       latter shadows the former. *)
    let extend : 'a t -> 'a t -> 'a t
      = fun m0 m1 ->
      if m0 == StringMap.empty then m1
      else if m1 == StringMap.empty || m0 == m1 then m0
      else StringMap.union (fun _ _ x -> Some x) m0 m1
  end

  type var = int
  type t = { vars: var StringMap.t }

  let empty : t = { vars = StringMap.empty }

  let bind_var : string -> var -> t -> t
    = fun name var env ->
    { vars = StringMap.add name var env.vars }

  let extend : t -> t -> t
    = fun env env' ->
    { vars = StringMap.extend env.vars env'.vars }
end

module Resolve = struct

  let resolve =
    object(self : 'self_type)
      inherit SugarTraversals.map as _super

      method bindings : Sugartypes.binding list -> Sugartypes.binding list
        = fun bs -> List.map self#binding bs
  end

  let program program = resolve#program program

  let sentence : Sugartypes.sentence -> Sugartypes.sentence = let open Sugartypes in function
  | Definitions bs     -> Definitions (resolve#bindings bs)
  | Expression exp     -> Expression  (resolve#phrase exp)
  | (Directive _) as d -> d
end

module Untyped = struct
  open Transform.Untyped

  let name = "resolveNames"

  let program state program =
    let program' = Resolve.program program in
    return state program'

  let sentence state sentence =
    let sentence' = Resolve.sentence sentence in
    return state sentence'
end
