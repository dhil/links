(* An abstraction for locating and importing compilation units. *)
open Utility

let normalise component = String.uncapitalize_ascii component
let to_filename components = String.concat Filename.dir_sep components
let links_ext = ".links"
let dir_sep = Str.regexp Filename.dir_sep
let split_filename filename =
  let components = Str.split dir_sep filename in
  if Filename.is_relative filename then components
  else "" :: components

module Root = struct
  type t = { hash: int; components: string list }

  let of_filename filename =
    let hash = Hashtbl.hash (Filename.absolute_path filename) in
    let components = split_filename filename in
    { hash; components }

  let of_filenames : ?with_zeroth:bool -> string list -> t array
    = fun ?(with_zeroth=false) roots ->
    (* let rec unduplicate roots abs_roots =
     *   match roots, abs_roots with
     *   | [], [] -> []
     *   | _ :: roots, abs_root :: abs_roots when List.mem abs_root abs_roots ->
     *      unduplicate roots abs_roots
     *   | root :: roots, _ :: abs_roots ->
     *      of_filename root :: unduplicate roots abs_roots
     *   | _ -> assert false
     * in *)
    let roots = List.map (fun root -> of_filename root) roots in
    Array.of_list (if with_zeroth then { hash = 0; components = [""] } :: roots else roots)

  let to_filename { components; _ } =
    to_filename components

  let to_strings { components; _ } = components

  let to_hash { hash; _ } = hash

  let hash : string -> int
    = fun filename -> Hashtbl.hash filename

  let same_as : t -> int -> bool =
    fun { hash; _ } otherhash -> hash = otherhash
end

module Cursor: sig
  type 'a t
  type rooted
  type unrooted

  val to_filename : 'a t -> string
  val of_filename : string -> unrooted t

  val of_strings : string list -> unrooted t
  val to_strings : 'a t -> string list
  val relativise : 'a t -> string list

  val root : Root.t -> unrooted t -> rooted t
  val unroot : rooted t -> unrooted t

  val to_qualified_name : 'a t -> string
end = struct
  type _ t =
    | Unrooted : string list -> unrooted t
    | Rooted : rooted -> rooted t
  and unrooted = string list
  and rooted = { root: Root.t; cursor: unrooted t }

  let rec to_filename : type a. a t -> string = function
    | Unrooted components ->
       String.concat Filename.dir_sep components ^ links_ext
    | Rooted { root; cursor } ->
       Filename.concat
         (Root.to_filename root)
         (to_filename cursor)

  let of_filename filename =
    if Filename.check_suffix filename links_ext then
      Unrooted (split_filename (Filename.chop_extension filename))
    else raise (Invalid_argument filename)

  let of_strings components = Unrooted components
  let rec to_strings : type a. a t -> string list = function
    | Unrooted components -> components
    | Rooted { root; cursor } ->
       Root.to_strings root @ to_strings cursor

  let root root cursor = Rooted { root; cursor }
  let unroot : rooted t -> unrooted t
    = fun (Rooted { cursor; _ }) -> cursor

  let rec relativise : type a. a t -> string list = function
    | Unrooted components -> components
    | Rooted { cursor; _ } -> relativise cursor

  let to_qualified_name : type a. a t -> string
    = fun cursor ->
    String.concat "." (List.map String.capitalize_ascii (relativise cursor))
end
type rooted = Cursor.rooted
type unrooted = Cursor.unrooted

module PhysicalNamespace: sig
  type t

  val of_directories : string list -> t
  val find : unrooted Cursor.t -> t -> rooted Cursor.t list
  val canonicalise : string -> t -> rooted Cursor.t list
end = struct
  type root_ptr = int
  type t =
    { roots: Root.t array;
      mutable space: root_ptr list StringTrie.t;
      mutable scanned: bool }

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

  let scan_depth = 1

  let if_absent x = [x]
  let if_present x xs =
    if List.mem x xs then xs
    else x :: xs

  let rec scan_dir : int -> t -> root_ptr -> string list -> string -> unit
    = fun depth ns root prefix cwd ->
    if depth < scan_depth then
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

  let[@ocaml.inline always] scan : t -> unit
    = fun ns ->
    if ns.scanned then ()
    else (* Skip the zeroth root. *)
      let num_roots = Array.length ns.roots - 1 in
      for i = 1 to num_roots do
        scan_dir 0 ns i [] (Root.to_filename ns.roots.(i))
      done; ns.scanned <- true; print ns

  let of_directories roots =
    let roots = Root.of_filenames ~with_zeroth:true roots in
    { roots; space = StringTrie.empty; scanned = false }

  let[@ocaml.inline always] resolve root { roots; _ } = roots.(root)

  let find cursor ns =
    (scan[@ocaml.inlined]) ns;
    try
      let roots = StringTrie.find (Cursor.to_strings cursor) ns.space in
      List.fold_left
        (fun rcursors root ->
          Cursor.root ((resolve[@ocaml.inlined]) root ns) cursor :: rcursors)
        [] roots
    with Notfound.NotFound _ -> []

  (* The following routine relies on [scan_depth] = 1. If we ever
     support deeper namespaces then this code needs to be slightly
     generalised to do a prefix match on roots rather than comparing
     the hashes of their canonical name. *)
  exception Match of Root.t
  let canonicalise file ns =
    if not (Sys.file_exists file) then []
    else begin
        ignore (if not (Filename.check_suffix file links_ext) then raise (Invalid_argument file));
        let hashed_dirname = Root.hash (Filename.absolute_path (Filename.dirname file)) in
        let num_roots = Array.length ns.roots - 1 in
        match
          for i = 1 to num_roots do
            let root = ns.roots.(i) in
            if Root.same_as root hashed_dirname then raise (Match ns.roots.(i))
          done
        with
        | () ->
           (* Make debug *)
           Printf.printf "No corresponding root\n%!";
           (* No corresponding root. *)
           let cursor = Cursor.of_filename file in
           let prefix = "" :: (Cursor.to_strings cursor) in
           ns.space <- StringTrie.add prefix [0] ns.space;
           [Cursor.root ns.roots.(0) cursor]
        | exception (Match root) ->
           (* TODO make debug *)
           Printf.printf "Corresponding root: %s\n%!" (Root.to_filename root);
           (* Found a corresponding root *)
           let basename = Filename.chop_extension (Filename.basename file) in
           find (Cursor.of_strings [basename]) ns
      end

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

  type t =
    { mutable loaded: Compilation_unit.t StringTrie.t;
      index: PhysicalNamespace.t }

  let dump : out_channel -> t -> unit
    = fun oc { loaded; _ } ->
    StringTrie.fold
      (fun prefix comp_unit () ->
        let prefix = String.concat "." prefix in
        Printf.fprintf oc "%s: %s\n%!" prefix (Compilation_unit.to_string comp_unit))
    loaded ()

  let make : ?path:string list -> unit -> t
    = fun ?(path=[]) () ->
    { loaded = StringTrie.empty;
      index  = PhysicalNamespace.of_directories path }

  let locate : unrooted Cursor.t -> t -> rooted Cursor.t list
    = fun cursor { index; _ } ->
    PhysicalNamespace.find cursor index

  let normalise prefix = List.map String.uncapitalize_ascii prefix

  let make_preload_obj () =
    let open SourceCode in
    object(self : 'self_type)
      inherit SugarTraversals.map as _super

      val imports   : Position.t StringTrie.t ref = ref StringTrie.empty
      val redundant : Position.t list StringTrie.t ref = ref StringTrie.empty

      method reset =
        imports := StringTrie.empty;
        redundant := StringTrie.empty
      method add import pos =
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
          | { node = Import name; pos } :: bs ->
             self#add (normalise [name]) pos;
             WithPos.make ~pos (QualifiedImport [name]) :: collect_imports bs
          | b :: bs -> b :: collect_imports bs
        in
        (collect_imports bs, exp)
    end

  (* Attempts to resolve a symbolic import to a physical file name. *)
  let resolve : SourceCode.Position.t StringTrie.t -> t -> (rooted Cursor.t * SourceCode.Position.t) list
    = fun imports st ->
    StringTrie.fold
      (fun prefix pos rcursors ->
        let cursor = Cursor.of_strings prefix in
        match locate cursor st with
        | [] -> raise (NonexistentDependency (Cursor.to_qualified_name cursor, pos))
        | [rcursor] -> (rcursor, pos) :: rcursors
        | rcursor :: ((_ :: _) as candidates) ->
           (* TODO emit warning. *)
           Printf.fprintf stderr "Warning: Ambiguous import for %s\n%!" (Cursor.to_qualified_name cursor);
           Printf.fprintf stderr "disambiguating by arbitrarily picking %s; alternatives were:\n%!" (Cursor.to_filename rcursor);
           List.iter
             (fun candidate ->
               Printf.fprintf stderr "   %s\n%!" (Cursor.to_filename candidate))
             candidates;
           (rcursor, pos) :: rcursors)
      imports []

  let rec load = fun visitor rcursor pos st ->
    let open Compilation_unit in
    let raw_cursor = Cursor.to_strings rcursor in
    (* Check whether the compilation unit pointed to by [rcursor] has
       already been loaded... *)
    try
      let comp_unit = StringTrie.find raw_cursor st.loaded in
      (* ... it may be in preload state, in which case we got ourselves a cyclic dependency. *)
      if not (Compilation_unit.is_loaded comp_unit) then raise (CyclicDependency ("FIXME" , pos))
      else comp_unit
    with Notfound.NotFound _ ->
      (* ... otherwise continue to process compilation unit induced by [rcursor]. *)
      let comp_unit = Compilation_unit.empty () in
      (* Add the compilation unit to the loader state. *)
      st.loaded <- StringTrie.add raw_cursor comp_unit st.loaded;
      (* Parse the file. *)
      let (ast, pos_ctxt) = P.parse (Cursor.to_filename rcursor) in
      (* Get the imports. *)
      ignore(visitor#reset);
      let ast = visitor#program ast in
      let rooted_cursors = resolve (visitor#get_imports) st in
      (* Printf.printf "|rooted_cursors| = %d, |visitor#get_imports| = %d\n%!" (List.length rooted_cursors) (StringTrie.size visitor#get_imports); *)
      let _redundant_imports = visitor#get_redundant_imports in
      (* Recursively attempt to load each dependency. *)
      let linkset = load_dependencies visitor rooted_cursors st StringTrie.empty in
      (* Promote the compilation unit. *)
      comp_unit.linkset <- linkset;
      comp_unit.source  <- (ast, pos_ctxt);
      st.loaded <- StringTrie.add raw_cursor (Compilation_unit.promote comp_unit) st.loaded;
      (* TODO FIXME: handle redundant imports. *)
      comp_unit
  and load_dependencies visitor rooted_cursors st linkset =
    match rooted_cursors with
    | [] -> linkset
    | (rooted_cursor, pos) :: rooted_cursors ->
       let comp_unit =
         try load visitor rooted_cursor pos st
         with e when is_import_error e ->
           raise (ImportError { source_file = Cursor.to_filename rooted_cursor; position = pos; error = e })
       in
       let linkset = StringTrie.add (Cursor.relativise rooted_cursor) comp_unit linkset in
       load_dependencies visitor rooted_cursors st linkset

  (* Attempts to (pre)load the compilation unit induced by the given
     file name. As a side effect, dependencies of the compilation unit
     will loaded transitively. *)
  let preload :  string -> t -> t
    = fun source_file st ->
    let visitor = make_preload_obj () in
    let main_cursor = match PhysicalNamespace.canonicalise source_file st.index with
      | [] -> failwith ("No such file " ^ source_file)
      | [x] -> Printf.printf "file: %s\n%!" (Cursor.to_filename x); x
      | _   -> failwith ("Multiple matches for " ^ source_file)
    in
    ignore (load visitor main_cursor SourceCode.Position.dummy st); st

  let compute_load_order : t -> (string * Compilation_unit.t) list
    = fun st ->
    let open Compilation_unit in
    let rec visit prefix comp_unit loaded order =
      match to_colour comp_unit with
      | Grey -> raise (Invalid_argument "Cycle detect")
      | Black -> order
      | White ->
         paint comp_unit Grey;
         let order =
           StringTrie.fold
             (fun prefix comp_unit order ->
               visit prefix comp_unit loaded order)
             comp_unit.linkset order
         in
         paint comp_unit Black;
         (String.concat "." prefix, comp_unit) :: order
    in
    StringTrie.fold
      (fun prefix comp_unit order ->
        match to_colour comp_unit with
        | White -> visit prefix comp_unit st.loaded order
        | _ -> order)
      st.loaded []
end

(* Load order abstraction *)
(* TODO: make [compute_load_order] return an instance of this. *)
module LoadOrder = struct
  type t = (string * Compilation_unit.t) list

  let load : (t -> 'a -> 'a) -> t list -> 'a -> 'a
    = List.fold_right
end
