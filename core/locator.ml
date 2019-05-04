(* An abstraction for locating and importing compilation units. *)
open Utility

(* module File = struct
 *   type t = File of string
 *          | Directory of string
 * 
 *   let map f = function
 *     | File s -> File (f s)
 *     | Directory s -> Directory (f s)
 * 
 *   let eject = function
 *     | File s | Directory s -> s
 * 
 *   let map_eject f file = eject (map f file)
 * 
 *   let basename : t -> string
 *     = fun x -> Filename.basename (eject x)
 * 
 *   let dirname : t -> string
 *     = fun x -> Filename.dirname (eject x)
 * 
 *   let is_relative : t -> bool
 *     = fun x -> Filename.is_relative (eject x)
 * 
 *   let is_absolute : t -> bool
 *     = fun x -> not (is_relative x)
 * end *)

module PhysicalNamespace: sig
  type t

  val of_directories : string list -> t
  val find : string list -> t -> string list
end = struct
  type root = int
  type t =
    { roots: string array;
      mutable space: root list StringTrie.t }

  let normalise = ref true
  let suffix = ".links"
  let namespace_depth_limit = 5

  let normalise file =
    if !normalise then String.capitalize_ascii file
    else file

  let if_absent x = [x]
  let if_present x xs =
    if List.mem x xs then xs
    else x :: xs

  let rec scan_dir : int -> t -> root -> string list -> string -> unit
    = fun depth ns root prefix cwd ->
    if depth < namespace_depth_limit then
      let is_dir = try Sys.is_directory cwd with Sys_error _ -> false in
      if not (is_dir) then failwith "no such directory" (* TODO issue warning *)
      else index_files depth ns root prefix cwd (Sys.readdir cwd)
  and index_files depth ns root prefix cwd files =
    let num_files = Array.length files - 1 in
    for i = 0 to num_files do
      let file = Array.get files i in
      index_file depth ns root prefix file cwd
    done
  and index_file depth ns root prefix file cwd =
    let file' = Filename.concat cwd file in
    if String.is_uncapitalized_ascii file
       && not (Unix.is_symlink file') then
         if Sys.is_directory file' then
           (* Scan the sub directory. *)
           scan_dir (depth + 1) ns root (file :: prefix) file'
         else if Filename.check_suffix file suffix then
           (* Add this file to the namespace [ns] with origin [root]. *)
           let basename = Filename.chop_suffix file suffix in
           ns.space <- StringTrie.add'
                         ~if_absent ~if_present
                         (List.rev (basename :: prefix)) root ns.space
         else () (* ignore file. *)
       else () (* ignore file. *)

  let print ns =
    let build prefix root =
      let root = Array.get ns.roots root in
      Printf.sprintf "%s%s" (Filename.concat root (String.concat Filename.dir_sep prefix)) suffix
    in
    StringTrie.iter
      (fun prefix roots ->
        List.iter (fun root -> Printf.printf "%s\n%!" (build prefix root)) roots)
      ns.space

  let of_directories : string list -> t
    = fun roots ->
    let roots =
      let roots' = ListUtils.unduplicate String.equal (List.map Filename.absolute_path roots) in
      Array.of_list roots'
    in
    let num_roots = Array.length roots - 1 in
    let namespace = { roots; space = StringTrie.empty } in
    for i = 0 to num_roots do
      scan_dir 0 namespace i [] roots.(i)
    done;
    print namespace; namespace

  let find : string list -> t -> string list
    = fun target -> assert false
end

module Compilation_unit = struct
  type envs = unit
  type 'a t = {
      filename: string;
      source: 'a;
      envs: envs;
      aliens: string list
    }

  let make : ?filename:string -> ?aliens:string list -> 'a -> 'a t
    = fun ?(filename="/dev/null") ?(aliens=[]) source ->
    { filename; source; envs = (); aliens }
end

module type PARSER = sig
  val parse : string -> Sugartypes.program * Scanner.position_context
end

module Make(P : PARSER) = struct
  exception CyclicDependency of string * SourceCode.Position.t

  type comp_unit = (Sugartypes.program * Scanner.position_context) Compilation_unit.t

  type t = {
    loaded: comp_unit StringMap.t; (* local cache. *)
    index: PhysicalNamespace.t Lazy.t;
  }

  let make : ?path:string list -> unit -> t
    = fun ?(path=[]) () ->
    { loaded = StringMap.empty;
      index  = lazy (PhysicalNamespace.of_directories path) }

  let locate : string -> t -> string list
    = fun target { index; _ } ->
    (* First try loading the file from the current loading directory,
       if it fails, then try querying the directory index.  *)
    PhysicalNamespace.find [target] (Lazy.force index)

  let induced_filename : string -> string
    = fun src_name -> Printf.sprintf "%s.links" (String.uncapitalize_ascii src_name)

  let source_name : string -> string
    = fun filename -> String.capitalize_ascii Filename.(chop_extension (basename filename))

  let make_preload_obj () =
    let open SourceCode in
    object(self : 'self_type)
      inherit SugarTraversals.map as super

      val imports   : Position.t StringTrie.t ref = ref StringTrie.empty
      val redundant : Position.t list StringTrie.t ref = ref StringTrie.empty

      method reset =
        imports := StringTrie.empty;
        redundant := StringTrie.empty
      method add qname pos =
        let import = QualifiedName.to_path qname in
        if StringTrie.mem import !imports
        then try
            let same = StringTrie.find import !redundant in
            redundant := StringTrie.add import (pos :: same) !redundant
            with Not_found -> redundant := StringTrie.add import [pos] !redundant
        else imports := StringTrie.add import pos !imports
      method get_imports = !imports
      method get_redundant_imports = !redundant

      method! program (bs, exp) =
        let open SourceCode.WithPos in
        let open Sugartypes in
        (* The function [process_imports] handles global imports. As a
           side-effect it rewrites the AST to remove the handled
           import nodes. *)
        let rec process_imports = function
          | [] -> []
          | { node = Import import; pos } :: bs when Import.is_global import ->
             self#add (Import.as_qualified_name import) pos;
             process_imports bs
          | b :: bs -> b :: process_imports bs
        in
        (process_imports bs, exp)
    end

  (* Attempts to (pre)load the compilation unit induced by the given
     file name. As a side effect, dependencies of the compilation unit
     will loaded transitively. *)
  let preload : string -> t -> t
    = fun file st ->
    let visitor = make_preload_obj () in
    (* let rec depth_first_load visitor file pos active st =
     *   assert false
     *   (\* let comp_unit_name = source_name file in
     *    * if StringSet.mem comp_unit_name active
     *    * then raise (CyclicDependency (comp_unit_name, pos))
     *    * else if StringMap.mem comp_unit_name st.loaded
     *    * then (active, st)
     *    * else let active = StringSet.add comp_unit_name active in
     *    *      let target = match locate file st with
     *    *        | [] -> failwith ("Cannot locate " ^ file)
     *    *        | [f] -> f
     *    *        | (f :: _) as candidates ->
     *    *           Printf.fprintf stderr "Ambiguous file, arbitrarily picking %s\n" f; f
     *    *      in
     *    *      let (ast, pos_ctxt) = P.parse target in
     *    *      ignore(visitor#reset);
     *    *      let ast = visitor#program ast in
     *    *      let source = (ast, pos_ctxt) in
     *    *      (\\* Hypothesis: The import set is typically small. *\\)
     *    *      let _redundant_imports = visitor#get_redundant_imports in
     *    *      match visitor#get_imports with
     *    *      | [] -> (\\* DONE *\\)
     *    *         let comp_unit = Compilation_unit.make ~filename:target source in
     *    *         (\\* TODO: mapping the basename of the filename to the
     *    *            source name is not quite adequate if we want
     *    *            namespaces. *\\)
     *    *         let comp_unit_name = source_name file in
     *    *         let active' = StringSet.remove comp_unit_name active in
     *    *         (active', { st with loaded = StringMap.add comp_unit_name comp_unit st.loaded })
     *    *      | imports -> assert false *\)
     * in
     * snd (depth_first_load visitor file SourceCode.Position.dummy StringSet.empty st) *)
    ignore (locate file st); assert false
end

(* module LinkSet: sig
 *   type 'a t = ('a Compilation_unit.t * string list) StringMap.t
 * 
 *   val empty : 'a t
 *   val link : 'a Compilation_unit.t
 * end = struct
 *   type 'a t = ('a Compilation_unit.t * string list) StringMap.t
 * end *)
