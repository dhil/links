open Utility
open Sugartypes
open Operators

module type BINDING_GROUPS = sig
  type t

  val of_bindings : binding list -> t
  val to_bindings : t -> binding list

  val transform_functions : (binding list -> binding list) -> t -> t
  val transform_typenames : (binding list -> binding list) -> t -> t
end

module rec BindingGroups : BINDING_GROUPS = struct
  type t =
    { cur_grp: [`Functions | `Modules | `Typenames | `Values | `None ];
      functions: (int * binding list) list;
      typenames: (int * binding list) list;
      values: (int * binding) list;
      modules: (int * (Sugartypes.name * position * t)) list }
  (* Needs to be generalised if we ever decide to have recursive
     modules. *)

  let index : (int * 'a) list -> int = function
    | (i, _) :: _ -> i
    | _ -> -1

  let value_index : t -> int
    = fun t -> index t.values

  let typename_index : t -> int
    = fun t -> index t.typenames

  let function_index : t -> int
    = fun t -> index t.functions

  let module_index : t -> int
    = fun t -> index t.modules

  let empty : t =
    { cur_grp = `None;
      functions = [];
      typenames = [];
      values = [];
      modules = [] }

  let next_index : t -> int
    = fun grps ->
    let i = function_index grps in
    let j = typename_index grps in
    let k = value_index grps in
    let m = module_index grps in
    let n =
      max (max i j) (max m k)
    in
    if n < 0 then 0 else n+1

  let apply : (binding list -> binding list) -> (int * binding list) list -> (int * binding list) list
    = fun transform bss ->
    List.map (fun (n, bs) -> (n, transform bs)) bss

  let add b : (int * binding list) list -> (int * binding list) list = function
    | [] -> assert false
    | (n, bs) :: bss -> (n, b :: bs) :: bss

  let add_value_binding b grps =
    let k = next_index grps in
    { grps with values = (k, b) :: grps.values; cur_grp = `Values }

  let add_module_binding b grps =
    let m = next_index grps in
    match b.node with
    | `Module (name, bindings) ->
       { grps with modules = (m, (name, b.pos, BindingGroups.of_bindings bindings)) :: grps.modules; cur_grp = `Modules }
    | _ -> assert false

  let add_typename_binding b grps =
    match grps.cur_grp with
    | `Typenames -> { grps with typenames = add b grps.typenames }
    | _ ->
       let j = next_index grps in
       let grp = ((j, [b]) :: grps.typenames) in
       { grps with typenames = grp; cur_grp = `Typenames }

  let add_function_binding b grps =
    match grps.cur_grp with
    | `Functions -> { grps with functions = add b grps.functions }
    | _ ->
       let i = next_index grps in
       let grp = ((i, [b]) :: grps.functions) in
       { grps with functions = grp; cur_grp = `Functions }


  let add : binding -> t -> t
    = fun b grps ->
    (* Printf.fprintf stderr "Adding: %s\n%!" (Sugartypes.show_binding b); *)
    match b.node with
    | `Handler _
    | `AlienBlock _ -> assert false (* Desugared at this point. *)
    | `Funs _ -> assert false (* Not yet introduced. *)
    | `QualifiedImport _
    | `Exp _
    | `Foreign _
    | `Val _ -> add_value_binding b grps
    | `Module _ -> add_module_binding b grps
    | `Type _ -> add_typename_binding b grps
    | `Fun _ -> add_function_binding b grps
    | `Infix -> grps (* discard binding *)

  let transform_modules : ((binding list -> binding list) -> t -> t) -> (binding list -> binding list) -> t -> t
    = fun apply transform grps ->
    { grps with modules = List.map (fun (i, (name, pos, grps)) -> (i, (name, pos, apply transform grps))) grps.modules }

  let transform_typenames : (binding list -> binding list) -> t -> t
    = fun transform grps ->
    let typenames = apply transform grps.typenames in
    let modules =
      transform_modules BindingGroups.transform_functions transform grps
    in
    { modules with typenames }

  let transform_functions : (binding list -> binding list) -> t -> t
    = fun transform grps ->
    let functions = apply transform grps.functions in
    let modules =
      transform_modules BindingGroups.transform_functions transform grps
    in
    { modules with functions }

  let to_bindings : t -> binding list
    = fun grps ->
    let pop_max : t -> (binding list * t) option =
      fun grps ->
      let i = function_index grps in
      let j = typename_index grps in
      let k = value_index grps in
      let m = module_index grps in
      if i > j && i > k && i > m
      then match grps.functions with
           | (_, fs) :: fss ->
              Some (fs, { grps with functions = fss })
           | [] -> None
      else if j > i && j > k && j > m
      then match grps.typenames with
           | (_, ts) :: tss ->
              Some (ts, { grps with typenames = tss })
           | [] -> None
      else if k > i && k > j && k > m
      then match grps.values with
           | (_, v) :: vs ->
              Some ([v], { grps with values = vs })
           | [] -> None
      else match grps.modules with
           | (_, (name, pos, grps)) :: ms ->
              let module' =
                with_pos pos (`Module (name, BindingGroups.to_bindings grps))
              in
              Some ([module'], { grps with modules = ms })
           | [] -> None
    in
    let rec build : binding list list -> t -> binding list list =
      fun bss grps ->
      match pop_max grps with
      | None -> bss
      | Some (bs, grps) ->
         let bss' = (List.rev bs) :: bss in
         build bss' grps
    in
    List.concat (build [] grps)

  let of_bindings : binding list -> t
    = fun bs ->
    List.fold_left
      (fun grps b -> add b grps)
      empty bs
end

module RecursiveFunctions = struct
  type sparse_graph = (string * string list) list
  type fun_def = Sugartypes.fun_def
  type fndata =
    { fundefs: (string, ((fun_def * Sugartypes.position) * StringSet.t)) Hashtbl.t;
      callgraph: sparse_graph }

  let compute_data : binding list -> fndata
    = fun bs ->
    let (fn_names', fundefs) =
      let handle_binding (fn_names, fundefs) b =
        match b.node with
        | `Fun ( (binder, _, (_, funlit), _, _) as def) ->
           let name = fst binder.node in
           let fvs = Freevars.funlit funlit in
           Hashtbl.add fundefs name ((def, b.pos), fvs);
           (name :: fn_names, fundefs)
        | _ -> assert false
      in
      List.fold_left
        handle_binding
        ([], Hashtbl.create 16) bs
    in
    (* compute call graph *)
    let callgraph =
      let fn_names = StringSet.from_list fn_names' in
      List.fold_left
        (fun adj name ->
          let freevars = snd (Hashtbl.find fundefs name) in
          (name, StringSet.(elements (inter freevars fn_names))) :: adj)
        [] fn_names'
    in
    { fundefs; callgraph }

  let group : binding list -> binding list
    = fun bs ->
    let data = compute_data bs in
    let sccs = Graph.topo_sort_sccs data.callgraph in
    let fundef name = fst (Hashtbl.find data.fundefs name) in
    let to_rec_def ((b, lin, (tyvars, funlit), loc, dt), pos) =
      (b, lin, ((tyvars, None), funlit), loc, dt, pos)
    in
    let is_recursive (b, _, _, _, _) =
      let name = fst b.node in
      let freevars = snd (Hashtbl.find data.fundefs name) in
      StringSet.mem name freevars
    in
    let make_funs names =
      let defs = List.map fundef names in
      match defs with
      | [def, pos] when not (is_recursive def) ->
         with_pos pos (`Fun def)
      | _ ->
         with_pos Sugartypes.dummy_position (`Funs (List.map to_rec_def defs))
    in
    List.rev_map make_funs sccs
end

let refine_bindings : binding list -> binding list
  = fun bs ->
  let grps = BindingGroups.of_bindings bs in
  let bs' =
    grps
    |> BindingGroups.transform_functions RecursiveFunctions.group
    (* |> BindingGroups.transform_typenames (fun _ -> assert false) *)
    |> BindingGroups.to_bindings
  in
  bs'

(****)
(* Helper function: add a group to a list of groups *)
let add group groups = match group with
  | [] -> groups
  | _  -> List.rev group::groups

(*
  * We need three traversals:
  * 1) Fold: find all type references within a type.
  * 2) Map: Replace all type applications of one name with another name
  * 3) Map: Inline a type application with a type.
*)
let find_type_references =
object (self)
  inherit SugarTraversals.fold as super

  val references : string list = []
  method add x = {< references = x :: references >}

  method references =
    StringSet.elements (StringSet.from_list (List.rev references))

  method! datatypenode = function
    | `TypeApplication (tyAppName, argList) ->
          let o =
            List.fold_left (fun acc ta -> acc#type_arg ta) self argList
          in
            o#add tyAppName
    | x -> super#datatypenode x

  method! row_var = function
    | `Open (x, _, _) -> self#add x
    | `Recursive (x, row) ->
        let o = self#add x in o#row row
    | x -> super#row_var x

end

let findTyRefs ty =
  (find_type_references#datatype ty)#references


(* Type application substitution *)
let subst_ty_app refFrom refTo =
object(_self)
  inherit SugarTraversals.map as super

  method! datatypenode : datatypenode -> datatypenode = function
    | `TypeApplication (tyAppName, _) as tyApp ->
        if tyAppName = refFrom then `TypeVar (refTo, Some default_subkind, `Rigid)
        else super#datatypenode tyApp
    | dt -> super#datatypenode dt
end

let substTyApp ty refFrom refTo =
  (subst_ty_app refFrom refTo)#datatypenode ty


(* Type variable substitution *)
let subst_ty_var varFrom (taTo : type_arg) =
object(self)
  inherit SugarTraversals.map as super

  (* varFrom: Type variable to substitute from.
   *  - This is the one in the tyTy
   * taTo: Type arg to replace with.
   *  - This is the one found in the application
   *)

  method! datatypenode : datatypenode -> datatypenode =
    fun dt ->
      match dt with
        | `TypeVar (n, _, _) when n = varFrom ->
            (match taTo with
               | `Type {node = dtTo; _} -> dtTo
               | _ -> super#datatypenode dt)
        | `Forall (qs, {node = quantDt; pos}) ->
            (match taTo with
              | `Type {node = `TypeVar (n, _, _); _} ->
                  let qs' =
                    List.map (fun (tv, k, f as q) ->
                      if tv = varFrom then
                        (n, k, f)
                      else q) qs in `Forall (qs', with_pos pos (self#datatypenode quantDt))
              | _ -> super#datatypenode dt)
        | _ -> super#datatypenode dt

  method! fieldspec : fieldspec -> fieldspec =
    fun fs ->
      match fs with
        | `Var (n, _, _) when n = varFrom ->
            (match taTo with
              | `Presence (`Var _ as fsTo) -> fsTo
              | _ -> super#fieldspec fs)
        | _ -> super#fieldspec fs

  method! row_var : row_var -> row_var = function
    | `Open (n, _, _) as rv when n = varFrom ->
        (match taTo with
          | `Row (_, (`Open _ as rv2)) -> rv2
          | _ -> super#row_var rv)
    | rv -> super#row_var rv

end

let substTyArg varFrom taTo ty =
  (subst_ty_var varFrom taTo)#datatypenode ty

(* Type inlining *)
let inline_ty toFind inlineArgs toInline =
object(_self)
  inherit SugarTraversals.map as super

  method! datatypenode : datatypenode -> datatypenode =
    fun dt ->
      match dt with
        | `TypeApplication (tyAppName, argList) as tyApp ->
            if tyAppName = toFind then (* && List.length argList = 0 then *)
              (* Ok, so what we need to do:
                * We have a list of the type arguments of the type to inline,
                * and also have a list of type arguments within the type app.
                * What we need to do is for every ty arg in the type,
                * substitute it for the corresponding arg in the arg list.
                * Bit like a fold / zip. There's probably some funky
                * category theory name for it, but blah.
                *)
              if (List.length inlineArgs = List.length argList) then
                List.fold_right (fun ((from_arg, _, _), to_arg) ty ->
                  (* We only want to work with type / row / presence *variables* here *)
                    substTyArg from_arg to_arg ty
                  ) (List.combine inlineArgs argList) toInline
              else
                (* Arity error, let something else pick it up *)
                 tyApp
            else
              super#datatypenode dt
        | x -> super#datatypenode x

end

let inlineTy ty tyRef inlineArgs refinedTy =
  (inline_ty tyRef inlineArgs refinedTy)#datatypenode ty

(* Similar to refine_bindings, RefineTypeBindings.refineTypeBindings finds
 * sequences of mutually recursive types, and rewrites them as explicit mus. *)
module RefineTypeBindings = struct

  (* Type synonyms *)
  type type_name = string
  type type_ty = name * (quantifier * tyvar option) list * datatype'
  type mu_alias = string
  type reference_info = (type_name, (type_name list * bool * position)) Hashtbl.t
  type type_hashtable = (type_name, type_ty) Hashtbl.t

  (* Type synonyms for substitution environments *)
  type alias_env = (type_name * mu_alias) list

  (*
   * Split binding list into groups for the purposes of type refinement.
   * A "group" is defined as a block of type bindings uninterrupted by any
   * other bindings.
  *)
  let initialGroups : binding list -> binding list list =
    fun bindings ->
      let group, groups =
        List.fold_right (fun ({node=binding; _} as bind) (currentGroup, otherGroups) ->
	  match binding with
          | `Handler _  (* Desugared at this point *)
          | `Module _
          | `QualifiedImport _
          | `AlienBlock _
          | `Funs _ -> assert false
          | `Fun _
          | `Foreign _
          | `Val _
          | `Exp _
          | `Infix ->
              (* Collapse and start a new group *)
              ([], add [bind] (add currentGroup otherGroups))
          | `Type _ ->
              (* Add to this group *)
              (bind :: currentGroup, otherGroups)
        ) bindings ([], [])
      in add group groups

  (* typeReferences gets us a list of type names referenced by a given type. *)
  let typeReferences : type_ty -> type_hashtable -> type_name list =
    fun (_, _, (sugaredDT, _)) ht ->
      List.filter (fun x -> Hashtbl.mem ht x)
        (findTyRefs sugaredDT)

  (* Does a type refer to itself? *)
  let refersToSelf : type_ty -> type_name list -> bool =
    fun (name, tyVars, (_sugaredDT, _)) refs ->
      let qExists =
        List.exists (fun (quant, _) ->
            let (qName, _, _) = quant in name = qName
          ) tyVars in
      let selfInDT = List.exists (fun x -> x = name) refs in
        qExists || selfInDT

  (* Gets the name of a type. *)
  let getName : type_ty -> type_name =
    fun (name, _, _) -> name

  (* Gets the sugared datatype from a type binding. *)
  let getDT : type_ty -> datatype =
    fun (_, _, (dt, _)) -> dt

  (* Updates the datatype in a type binding. *)
  let updateDT : type_ty -> datatypenode -> type_ty =
    fun (name, tyArgs, ({pos; _}, unsugaredDT)) newDT ->
      (name, tyArgs, ((with_pos pos newDT), unsugaredDT))

  let referenceInfo : binding list -> type_hashtable -> reference_info =
    fun binds typeHt ->
      let ht = Hashtbl.create 30 in
      List.iter (fun {node = bind; pos} ->
        match bind with
          | `Type (name, _, _ as tyTy) ->
              let refs = typeReferences tyTy typeHt in
              let referencesSelf = refersToSelf tyTy refs in
              Hashtbl.add ht name (refs, referencesSelf, pos)
          | _ -> assert false;
      ) binds;
      ht

  let refGraph : reference_info -> (type_name * type_name list) list =
    fun riTable ->
      (* Massively irritating, there's no toList function... *)
      List.rev (Hashtbl.fold (fun k (adj, _, _) acc ->
        (k, adj) :: acc
      ) riTable [])

  let isSelfReferential : type_name -> reference_info -> bool =
    fun name riTable ->
      snd3 (Hashtbl.find riTable name)


  (* Performs the inlining transformation on a given type. *)
  let rec refineType :
      type_ty ->
      alias_env ->
      type_hashtable ->
      type_name list -> (* Other components in the SCC list *)
      reference_info ->
      type_ty =
    fun ty env ht sccs ri ->
      let tyName = getName ty in
      let rts = isSelfReferential tyName ri in
      let sugaredDT = getDT ty in
      (* (if (tyName = "Tree" || tyName = "Forest") then (
       *    Printf.fprintf stderr "==== before (%s) ====\n%!" tyName;
       *    Printf.fprintf stderr "%s\n%!" (Sugartypes.show_datatype sugaredDT))); *)
      (* If we're self-referential, then add in a top-level mu *)
      let (env', dt) =
        if List.mem_assoc tyName env then assert false else
        if rts || List.length sccs > 1 then
          let muName = gensym ~prefix:"refined_mu" () in
            ((tyName, muName) :: env, `Mu (muName, sugaredDT))
        else (env, sugaredDT.node) in
      (* Now, we go through the list of type references.
       * If the reference is in the substitution environment, we replace it
       * with the mu variable we've created.
       * If not, then we'll need to refine that type, and inline it.
       *)
      let refinedTy = List.fold_right (fun tyRef curDataTy ->
        (* Only perform this transformation on other types in the group, and to self if self-referential. *)
        let shouldApply = Hashtbl.mem ht tyRef && (tyName <> tyRef || rts) in
        if shouldApply then
          (if List.mem_assoc tyRef env' then
            (* Simple tyApp substitution *)
            let muName = List.assoc tyRef env' in
            substTyApp curDataTy tyRef muName
           else
            (* Otherwise, we'll need to refine and inline *)
               let to_refine = Hashtbl.find ht tyRef in
               let (_, arg_list, _) = to_refine in
               let to_refine_args = List.map fst arg_list in
               let (_, _, (refinedRef, _)) = refineType to_refine env' ht sccs ri in
               inlineTy curDataTy tyRef to_refine_args refinedRef.node)
        else
          curDataTy)
                        sccs dt
      in
      (* (if (tyName = "Tree" || tyName = "Forest") then
       *    (Printf.fprintf stderr "==== after (%s) ====\n%!" tyName;
       *     Printf.fprintf stderr "%s\n%!" (Sugartypes.show_datatype refinedTy))); *)
      updateDT ty refinedTy

  let refineSCCGroup :
      reference_info ->
      (type_name, type_ty) Hashtbl.t ->
      type_name list ->
      binding list =
    fun ri ht sccs ->
      let getPos name =
        thd3 (Hashtbl.find ri name) in
      List.map (fun name ->
        let res = refineType (Hashtbl.find ht name) [] ht sccs ri in
        with_pos (getPos name) (`Type res)
      ) sccs

  let isTypeGroup : binding list -> bool = function
    | {node = `Type _; _} :: _xs -> true
    | _ -> false

  (* Performs type refinement on a binding group. *)
  let refineGroup : binding list -> binding list = function
    | binds when isTypeGroup binds ->
      (* Create a hashtable mapping names to type bindings. *)
      let ht = Hashtbl.create 30 in
      List.iter (fun {node; _} ->
        match node with
          | `Type (name, _, _ as tyTy) ->
            Hashtbl.add ht name tyTy;
          | _ -> assert false;
      ) binds;
      let refInfoTable = referenceInfo binds ht in
      let graph = refGraph refInfoTable in
      let sccList = Graph.topo_sort_sccs graph in
      (* Irritatingly we need to reattach the bindings with a position *)
      List.concat (
        List.map
          (fun sccGroup ->
            refineSCCGroup refInfoTable ht sccGroup
          ) sccList
      )
    | xs -> xs
  (* Refines a list of bindings. *)
  let refineTypeBindings : binding list -> binding list =
    fun binds ->
      List.concat (List.map refineGroup (initialGroups binds))
end

let refine_bindings =
object (self)
  inherit SugarTraversals.map as super
  method! phrasenode : phrasenode -> phrasenode = function
    |`Block (bindings, body) ->
       let bindings = self#list (fun o -> o#binding) bindings in
       let body = self#phrase body in
       let refined_bindings =
         (RefineTypeBindings.refineTypeBindings ->-
         refine_bindings) bindings in
       `Block (refined_bindings, body)
    | p -> super#phrasenode p

  method! program : program -> program =
    fun (bindings, body) ->
      let bindings = self#list (fun o -> o#binding) bindings in
      let body = self#option (fun o -> o#phrase) body in
      let refined_bindings =
        (RefineTypeBindings.refineTypeBindings ->-
        refine_bindings) bindings in
      refined_bindings, body

  method! sentence : sentence -> sentence = function
    |`Definitions defs ->
       let defs = self#list (fun o -> o#binding) defs in
       let refined_bindings =
         (RefineTypeBindings.refineTypeBindings ->-
         refine_bindings) defs in
       `Definitions (refined_bindings)
    | d -> super#sentence d

end
