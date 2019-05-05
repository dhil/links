(* An abstraction for locating and importing compilation units. *)
open Utility

let normalise component = String.uncapitalize_ascii component
let to_filename components = String.concat Filename.dir_sep components
let links_ext = ".links"

module Root = struct
  type t = string list

  let dir_sep = Str.regexp Filename.dir_sep

  let of_filename filename =
    let components = Str.split dir_sep filename in
    if Filename.is_relative filename
    then "" :: components
    else components

  let of_filenames : string list -> t array
    = fun roots ->
    let rec unduplicate roots abs_roots =
      match roots, abs_roots with
      | [], [] -> []
      | _ :: roots, abs_root :: abs_roots when List.mem abs_root abs_roots ->
         unduplicate roots abs_roots
      | root :: roots, _ :: abs_roots ->
         of_filename root :: unduplicate roots abs_roots
      | _ -> assert false
    in
    Array.of_list (unduplicate roots (List.map Filename.absolute_path roots))

  let to_filename components =
    to_filename components

  let to_strings components = components
end

module Cursor: sig
  type 'a t
  type physical
  type rooted
  type unrooted

  val to_filename : 'a t -> string
  val of_filename : string -> physical t
  val of_strings : string list -> unrooted t
  val to_strings : 'a t -> string list
  val root : Root.t -> unrooted t -> rooted t
  val unroot : rooted t -> unrooted t
  val to_qualified_name : unrooted t -> QualifiedName.t
  val realise : rooted t -> physical t
end = struct
  type _ t =
    | Unrooted : string list -> unrooted t
    | Rooted : rooted -> rooted t
    | Physical : physical -> physical t
  and unrooted = string list
  and rooted = { root: Root.t; cursor: unrooted t }
  and physical = { proot: Root.t; pcursor: unrooted t }
  (* Hack: reusing the root structure even
     though a physical file may not necessary be
     a root in the name space sense. *)

  let empty = Unrooted []

  let rec to_filename : type a. a t -> string = function
    | Unrooted components ->
       String.concat Filename.dir_sep components ^ links_ext
    | Rooted { root; cursor } ->
       Filename.concat
         (Root.to_filename root)
         (to_filename cursor)
    | Physical { proot; pcursor } ->
       Filename.concat
         (Root.to_filename proot)
         (to_filename pcursor)


  let of_filename filename =
    let filename = Filename.chop_extension filename in
    Physical { proot = Root.of_filename filename; pcursor = empty }

  let of_strings components = Unrooted (List.map normalise components)
  let rec to_strings : type a. a t -> string list = function
    | Unrooted components -> components
    | Rooted { root; cursor } ->
       Root.to_strings root @ to_strings cursor
    | Physical { proot; pcursor } ->
       Root.to_strings proot @ to_strings pcursor
  let root root cursor = Rooted { root; cursor }
  let unroot : rooted t -> unrooted t
    = fun (Rooted { cursor; _ }) -> cursor

  let realise : rooted t -> physical t
    = fun (Rooted { root; cursor }) -> Physical { proot = root; pcursor = cursor }

  let to_qualified_name (Unrooted components) =
    QualifiedName.of_path components
end
type rooted = Cursor.rooted
type unrooted = Cursor.unrooted
type physical = Cursor.physical

module PhysicalNamespace: sig
  type t

  val of_directories : string list -> t
  val find : unrooted Cursor.t -> t -> rooted Cursor.t list
end = struct
  type root_ptr = int
  type t =
    { roots: Root.t array;
      mutable space: root_ptr list StringTrie.t }

  let namespace_depth_limit = 5

  let if_absent x = [x]
  let if_present x xs =
    if List.mem x xs then xs
    else x :: xs

  let rec scan_dir : int -> t -> root_ptr -> string list -> string -> unit
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
         else if Filename.check_suffix file links_ext then
           (* Add this file to the namespace [ns] with origin [root]. *)
           let basename = Filename.chop_suffix file links_ext in
           ns.space <- StringTrie.add'
                         ~if_absent ~if_present
                         (List.rev (basename :: prefix)) root ns.space
  (* otherwise ignore the file *)

  let print ns =
    let build prefix root =
      let root = Array.get ns.roots root in
      Printf.sprintf "%s%s" (Filename.concat
                               (Root.to_filename root)
                               (to_filename prefix)) links_ext
    in
    StringTrie.iter
      (fun prefix roots ->
        List.iter (fun root -> Printf.printf "%s\n%!" (build prefix root)) roots)
      ns.space

  let of_directories : string list -> t
    = fun roots ->
    let roots = Root.of_filenames roots in
    let num_roots = Array.length roots - 1 in
    let namespace = { roots; space = StringTrie.empty } in
    for i = 0 to num_roots do
      scan_dir 0 namespace i [] (Root.to_filename roots.(i))
    done;
    print namespace; namespace

  let resolve root { roots; _ } = roots.(root)

  let find : unrooted Cursor.t -> t -> rooted Cursor.t list
    = fun cursor ns ->
    try
      let roots = StringTrie.find (Cursor.to_strings cursor) ns.space in
      List.fold_left
        (fun rcursors root ->
          Cursor.root (resolve root ns) cursor :: rcursors)
        [] roots
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

exception ImportError of { source_file: string;
                           position: SourceCode.Position.t;
                           error: exn; }
exception CyclicDependency of string * SourceCode.Position.t
exception NonexistentDependency of string * SourceCode.Position.t
let is_import_error = function
  | ImportError _ | CyclicDependency _ | NonexistentDependency _ -> true
  | _ -> false

module Make(P : PARSER) = struct
  type comp_unit = (Sugartypes.program * Scanner.position_context) Compilation_unit.t

  type t = {
    loaded: comp_unit StringTrie.t; (* Local cache. *)
    index: PhysicalNamespace.t Lazy.t;
    mutable active: unit StringTrie.t;
  }

  let make : ?path:string list -> unit -> t
    = fun ?(path=[]) () ->
    { loaded = StringTrie.empty;
      index  = lazy (PhysicalNamespace.of_directories path);
      active = StringTrie.empty }

  let locate : unrooted Cursor.t -> t -> rooted Cursor.t list
    = fun cursor { index; _ } ->
    PhysicalNamespace.find cursor (Lazy.force index)

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
        (* The function [collect_imports] collects global imports. As a
           side-effect it rewrites the AST to remove the handled
           import nodes. *)
        let rec collect_imports = function
          | [] -> []
          | { node = Import import; pos } :: bs when Import.is_global import ->
             self#add (Import.as_qualified_name import) pos;
             collect_imports bs
          | b :: bs -> b :: collect_imports bs
        in
        (collect_imports bs, exp)
    end

  (* Attempts to resolve a symbolic import to a physical file name.
     Hypothesis: The [imports] set is typically small. *)
  let resolve imports st =
    StringTrie.fold
      (fun prefix pos rcursors ->
        let cursor = Cursor.of_strings prefix in
        match locate cursor st with
        | [] -> raise (NonexistentDependency ((String.concat "." prefix), pos))
        | [rcursor] -> (rcursor, pos) :: rcursors
        | rcursor :: (_ :: _) as candidates ->
           (* TODO emit warning. *)
           Printf.fprintf stderr "Warning: Ambiguous import for %s\n%!" (QualifiedName.to_string (Cursor.to_qualified_name cursor));
           Printf.fprintf stderr "disambiguating by arbitrarily picking %s; alternatives were:\n%!" (Cursor.to_filename rcursor);
           List.iter
             (fun candidate ->
               Printf.fprintf stderr "   %s\n%!" (Cursor.to_filename candidate))
             candidates;
           (rcursor, pos) :: rcursors)
      imports []

  let rec load visitor fcursor pos st =
    (* Check whether the file is currently opened by a parent process... *)
    let fcursor' = Cursor.to_strings fcursor in
    (if StringTrie.mem fcursor' st.active
     then raise (CyclicDependency ("FIXME" , pos)));
    (* ... otherwise mark it as being open. *)
    st.active <- StringTrie.add fcursor' () st.active;
    (* Parse the file. *)
    let (ast, pos_ctxt) = P.parse (Cursor.to_filename fcursor) in
    (* Get the imports. *)
    ignore(visitor#reset);
    let ast = visitor#program ast in
    let source = (ast, pos_ctxt) in
    let rooted_cursors = resolve visitor#get_imports st in
    (* Recursively attempt to load each dependency. *)
    let st = load_dependencies visitor st rooted_cursors in
    (* Unmark the file. *)
    st.active <- StringTrie.remove fcursor' st.active;
    (* Mark the file as loaded. *)
    (* TODO FIXME: handle redundant imports. *)
    assert false
  and load_dependencies visitor st rooted_cursors =
    match rooted_cursors with
    | [] -> st
    | (rooted_cursor, pos) :: rooted_cursors ->
       let st =
         try load visitor (Cursor.realise rooted_cursor) pos st
         with e when is_import_error e ->
           raise (ImportError { source_file = Cursor.to_filename rooted_cursor; position = pos; error = e })
       in load_dependencies visitor st rooted_cursors

  (* Attempts to (pre)load the compilation unit induced by the given
     file name. As a side effect, dependencies of the compilation unit
     will loaded transitively. *)
  let preload : string -> t -> t
    = fun source_file st ->
    let visitor = make_preload_obj () in
    load visitor (Cursor.of_filename source_file) SourceCode.Position.dummy st

end

(* module LinkSet: sig
 *   type 'a t = ('a Compilation_unit.t * string list) StringMap.t
 * 
 *   val empty : 'a t
 *   val link : 'a Compilation_unit.t
 * end = struct
 *   type 'a t = ('a Compilation_unit.t * string list) StringMap.t
 * end *)
