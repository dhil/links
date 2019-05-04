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
  type cursor = string list
  type rooted_cursor

  val of_directories : string list -> t
  val find : string list -> t -> rooted_cursor list
  val to_filename : rooted_cursor -> t -> string
  val to_cursor : rooted_cursor -> cursor
end = struct
  type cursor = string list
  type rooted_cursor = { root: int; cursor: cursor }
  type root = int
  type t =
    { roots: string array;
      mutable space: root list StringTrie.t }

  let suffix = ".links"
  let namespace_depth_limit = 5

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
  (* otherwise ignore the file *)

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

  let normalise component =
    String.uncapitalize_ascii component

  let to_filename { root; cursor } ns =
    let cursor = List.map normalise cursor in
    let root = ns.roots.(root) in
    (Filename.concat root (String.concat Filename.dir_sep cursor)) ^ suffix

  let to_cursor { cursor; _ } = cursor

  let find : string list -> t -> rooted_cursor list
    = fun cursor ns ->
    try
      let roots = StringTrie.find cursor ns.space in
      List.fold_left
        (fun rcursors root -> { root; cursor } :: rcursors) [] roots
    with Notfound.NotFound _ -> []
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
  exception ImportError of { source_file: string;
                             position: SourceCode.Position.t;
                             error: exn; }
  exception CyclicDependency of string * SourceCode.Position.t
  exception NonexistentDependency of string * SourceCode.Position.t

  let is_import_error = function
    | ImportError _ | CyclicDependency _ | NonexistentDependency _ -> true
    | _ -> false

  type comp_unit = (Sugartypes.program * Scanner.position_context) Compilation_unit.t

  type t = {
    loaded: comp_unit StringTrie.t; (* Local cache. *)
    index: PhysicalNamespace.t Lazy.t;
    active: (string, unit) Hashtbl.t (* Set of opened files. *)
  }

  let make : ?path:string list -> unit -> t
    = fun ?(path=[]) () ->
    { loaded = StringTrie.empty;
      index  = lazy (PhysicalNamespace.of_directories path);
      active = Hashtbl.create 32 }

  let locate : string list -> t -> PhysicalNamespace.rooted_cursor list
    = fun cursor { index; _ } ->
    PhysicalNamespace.find cursor (Lazy.force index)

  let to_filename : PhysicalNamespace.rooted_cursor -> t -> string
    = fun rcursor { index; _ } ->
    PhysicalNamespace.to_filename rcursor (Lazy.force index)

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
    = fun source_file st ->
    let visitor = make_preload_obj () in
    let rec load visitor file pos st =
      (* Check whether the file is currently opened by a parent process... *)
      (if Hashtbl.mem st.active file
       then raise (CyclicDependency (file, pos)));
      (* ... otherwise mark it as being open. *)
      Hashtbl.add st.active file ();
      (* Parse the file. *)
      let (ast, pos_ctxt) = P.parse file in
      (* Get the imports. *)
      ignore(visitor#reset);
      let ast = visitor#program ast in
      let source = (ast, pos_ctxt) in
      (* Hypothesis: The import set is typically small. *)
      let resolve imports =
        StringTrie.fold
          (fun prefix pos cursors ->
            match locate prefix st with
            | [] -> raise (NonexistentDependency ((String.concat "." prefix), pos))
            | [cursor] -> (cursor, pos) :: cursors
            | cursor :: (_ :: _) as candidates ->
               (* TODO emit warning. *)
               Printf.fprintf stderr "Warning: Ambiguous import for %s\n%!" (String.concat "." prefix);
               Printf.fprintf stderr "disambiguating by arbitrarily picking %s; alternatives were:\n%!" (to_filename cursor st);
               List.iter
                 (fun candidate ->
                   Printf.fprintf stderr "   %s\n%!" (to_filename candidate st))
                 candidates;
               (cursor, pos) :: cursors)
        imports []
      in
      let rooted_cursors = resolve visitor#get_imports in
      (* Recursively attempt to load each dependency. *)
      let rec load_dependencies visitor st rooted_cursors =
        match rooted_cursors with
        | [] -> st
        | (rooted_cursor, pos) :: rooted_cursors ->
           let st =
             let file = to_filename rooted_cursor st in
             try load visitor file pos st
             with e when is_import_error e ->
               raise (ImportError { source_file = file; position = pos; error = e })
           in load_dependencies visitor st rooted_cursors
      in
      let st = load_dependencies visitor st rooted_cursors in
      (* Unmark the file. *)
      Hashtbl.remove st.active file;
      (* Mark the file as loaded. *)
      (* TODO FIXME: handle redundant imports. *)
      assert false
    in
    load visitor source_file SourceCode.Position.dummy st
end

(* module LinkSet: sig
 *   type 'a t = ('a Compilation_unit.t * string list) StringMap.t
 * 
 *   val empty : 'a t
 *   val link : 'a Compilation_unit.t
 * end = struct
 *   type 'a t = ('a Compilation_unit.t * string list) StringMap.t
 * end *)
