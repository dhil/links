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
let namespace_depth_limit = 0

module Root = struct
  type t = { canonical: string list; original: string list; assumed: bool }

  let of_filename filename =
    let assumed = false in
    let original = split_filename filename in
    if Filename.is_relative filename then
      let canonical = split_filename (Filename.absolute_path filename) in
      { canonical; original; assumed }
    else
      { canonical = original; original; assumed }

  let of_filenames : string list -> t array
    = fun roots ->
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
    Array.of_list roots

  let to_filename { original; _ } =
    to_filename original

  let to_strings { original; _ } = original

  let is_prefix_of : t -> string list -> bool
    = fun { canonical; _ } canonical_components' ->
    (* Printf.printf "Comparing: [%s] and [%s]\n%!" (String.concat "." canonical) (String.concat "." canonical_components'); *)
    let rec compare relative_depth canonical canonical' = match canonical, canonical' with
      | [], _ :: canonical' ->
         if relative_depth <= namespace_depth_limit
         then compare (relative_depth + 1) canonical canonical'
         else false
      | [], [] -> relative_depth <= namespace_depth_limit
      | prefix :: canonical, prefix' :: canonical' ->
         if String.compare prefix prefix' = 0
         then compare relative_depth canonical canonical'
         else false
      | _, [] -> false
    in
    (* Allow one level of difference. *)
    let initial_diff = (-1) in
    compare initial_diff canonical canonical_components'

  let cut : t -> string list -> string list
    = fun { canonical; _ } canonical_components' ->
    let rec cut canonical canonical' = match canonical, canonical' with
      | [], canonical' -> canonical'
      | _, []          -> []
      | prefix :: canonical, (prefix' :: canonical' as result) ->
         if String.compare prefix prefix' = 0
         then cut canonical canonical'
         else result
    in
    cut canonical canonical_components'

  let assume filename =
    let root = of_filename filename in
    { root with assumed = true }

  let is_assumed { assumed; _ } = assumed
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
  val with_root : Root.t -> string -> rooted t

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

  let with_root root filename =
    let canonical_components =
      split_filename (Filename.absolute_path (Filename.chop_extension filename))
    in
    Rooted { root; cursor = of_strings (Root.cut root canonical_components) }

  let rec relativise : type a. a t -> string list = function
    | Unrooted components -> components
    | Rooted { cursor; _ } -> relativise cursor

  let to_qualified_name : type a. a t -> string
    = fun cursor ->
    String.concat "." (List.map String.capitalize_ascii (relativise cursor))
end
type rooted = Cursor.rooted
type unrooted = Cursor.unrooted

module Namespace: sig
  type t

  val of_directories : string list -> t
  val find : unrooted Cursor.t -> t -> rooted Cursor.t list
  val canonicalise : string -> t -> rooted Cursor.t list
  val assume : string -> t -> rooted Cursor.t
end = struct
  type root_ptr = int
  type t =
    { mutable roots: Root.t array;
      (* TODO FIXME: use a compact encoding based on powers of two. *)
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

  let if_absent x = [x]
  let if_present x xs =
    if List.mem x xs then xs
    else x :: xs

  let rec scan_dir : int -> t -> root_ptr -> string list -> string -> unit
    = fun depth ns root prefix cwd ->
    if depth <= namespace_depth_limit then
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
    else
      let num_roots = Array.length ns.roots - 1 in
      for i = 0 to num_roots do
        let root = ns.roots.(i) in
        if not (Root.is_assumed root) then
          scan_dir 0 ns i [] (Root.to_filename root)
      done; ns.scanned <- true; print ns

  let of_directories roots =
    let roots = Root.of_filenames roots in
    { roots; space = StringTrie.empty; scanned = false }

  let[@ocaml.inline always] resolve root { roots; _ } = roots.(root)

  let find cursor ns =
    (scan[@ocaml.inlined]) ns;
    try
      let roots = StringTrie.find (Cursor.to_strings cursor) ns.space in
      List.fold_left
        (fun rooted_cursors root ->
          Cursor.root ((resolve[@ocaml.inlined]) root ns) cursor :: rooted_cursors)
        [] roots
    with Notfound.NotFound _ -> []

  let assume file ns =
    let root = Root.assume (Filename.dirname file) in
    ns.roots <- Array.append ns.roots [|root|];
    let unit_name = [Filename.chop_extension (Filename.basename file)] in
    ns.space <- StringTrie.add unit_name [Array.length ns.roots - 1] ns.space;
    Cursor.root root (Cursor.of_strings unit_name)

  (* The following routine checks whether [file] lives in one of the
     name space roots. This is necessary to prevent a symbolic name in
     [file] to resolves to [file]. *)
  exception Match of Root.t
  let canonicalise file ns =
    if not (Sys.file_exists file) then []
    else begin
        ignore (if not (Filename.check_suffix file links_ext) then raise (Invalid_argument file));
        let canonical_components = split_filename (Filename.absolute_path (Filename.chop_extension file)) in
        let num_roots = Array.length ns.roots - 1 in
        match
          for i = 1 to num_roots do
            let root = ns.roots.(i) in
            if Root.is_prefix_of root canonical_components then raise (Match ns.roots.(i))
          done
        with
        | () ->
           (* Make debug *)
           Printf.printf "No corresponding root\n%!";
           (* No corresponding root. *)
           let rooted_cursor = assume file ns in
           (* let cursor = Cursor.of_filename file in
            * let prefix = "" :: (Cursor.to_strings cursor) in
            * ns.space <- StringTrie.add prefix [0] ns.space; *)
           [rooted_cursor]
        | exception (Match root) ->
           (* TODO make debug *)
           Printf.printf "Corresponding root: %s\n%!" (Root.to_filename root);
           (* Found a corresponding root *)
           let rooted_cursor = Cursor.with_root root file in
           find (Cursor.unroot rooted_cursor) ns
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
  (* Load store API. *)
  module type COMMON_STORE_API = sig
    type key
    type store
    type item

    val mem : key -> store -> bool
    val find : key -> store -> item
    val fold : (key -> item -> 'a -> 'a) -> store -> 'a -> 'a
  end

  module Store: sig
    type qualified_name = string list
    type t

    val empty : t
    val add : string -> qualified_name -> Compilation_unit.t -> t -> t
    val union_disjoint : t -> t ->t

    module ById : sig
      include COMMON_STORE_API with type store := t and type key := int and type item := Compilation_unit.t

      val as_qualified_name : int -> t -> qualified_name
    end
    module ByName : COMMON_STORE_API with type store := t and type key := qualified_name and type item := Compilation_unit.t
    (* module ByFilename : STORE_API with type store := t and type key := string and type item := Compilation_unit.t *)
  end = struct
    type qualified_name = string list
    type t =
      { by_id: Compilation_unit.t IntMap.t;
        by_name: Compilation_unit.t StringTrie.t;
        (* by_fname: Compilation_unit.t IntMap.t *) }

    let empty =
      { by_id = IntMap.empty;
        by_name = StringTrie.empty;
        (* by_fname = IntMap.empty *) }

    let add _filename prefix comp_unit ls =
      let open Compilation_unit in
      { by_id = IntMap.add comp_unit.id comp_unit ls.by_id;
        by_name = StringTrie.add prefix comp_unit ls.by_name;
        (* by_fname = IntMap.add (Hashtbl.hash filename) comp_unit ls.by_fname *) }

    let union_disjoint store store' =
      let by_id = IntMap.union_disjoint store.by_id store'.by_id in
      let by_name = StringTrie.union_disjoint store.by_name store'.by_name in
      { by_id; by_name }

    module ById = struct
      let find id { by_id; _ } = IntMap.find id by_id
      let mem id { by_id; _ } = IntMap.mem id by_id
      let fold f { by_id; _ } z = IntMap.fold f by_id z
      let as_qualified_name id { by_name; _ } =
        let exception Found of string list in
        try
          ignore
            (StringTrie.fold
               (fun prefix comp_unit id ->
                 if comp_unit.Compilation_unit.id = id then raise (Found prefix)
                 else id)
               by_name id);
        raise Not_found
        with Found prefix -> prefix
    end

    module ByName = struct
      let find prefix { by_name; _ } = StringTrie.find prefix by_name
      let mem prefix { by_name; _ } = StringTrie.mem prefix by_name
      let fold f { by_name; _ } z = StringTrie.fold f by_name z
    end

    (* module ByFilename = struct
     *   let find fname { by_fname; _ } = IntMap.find (Hashtbl.hash fname) by_fname
     *   let mem fname { by_fname; _ }  = IntMap.mem (Hashtbl.hash fname) by_fname
     *   let fold f { by_fname; _ } z   =
     *     IntMap.fold (fun key comp_unit z -> f (string_of_int key) comp_unit z) by_fname z
     * end *)
  end

  (* Load order abstraction *)
  module LoadOrder: sig
    type t

    val empty : Store.t -> t
    val add : int -> t -> t
    val load : (Compilation_unit.t -> Store.t -> 'a -> 'a) -> t -> 'a -> 'a
  end = struct
    type t = { store: Store.t;
               seq: int list }

    let empty store = { store; seq = [] }
    let add : int -> t -> t
      = fun id lo -> { lo with seq = id :: lo.seq }

    let load : (Compilation_unit.t -> Store.t -> 'a -> 'a) -> t -> 'a -> 'a
      = fun f { store; seq } z ->
      List.fold_right
        (fun id z ->
          let comp_unit = Store.ById.find id store in
          (* Skip synthetic units. *)
          if Compilation_unit.is_ready comp_unit
          then z
          else f comp_unit store z)
        seq z
  end

  type t =
    { mutable store: Store.t;
      index: Namespace.t }

  let dump : out_channel -> t -> unit
    = fun oc { store; _ } ->
    Store.ByName.fold
      (fun prefix comp_unit () ->
        let prefix = String.concat "." prefix in
        Printf.fprintf oc "%s: %s\n%!" prefix (Compilation_unit.to_string comp_unit))
    store ()

  let make : ?path:string list -> unit -> t
    = fun ?(path=[]) () ->
    { store = Store.empty;
      index = Namespace.of_directories path }

  let locate : unrooted Cursor.t -> t -> rooted Cursor.t list
    = fun cursor { index; _ } ->
    Namespace.find cursor index

  let normalise prefix = List.map String.uncapitalize_ascii prefix

  let make_preload_obj () =
    let open SourceCode in
    let open WithPos in
    let open Sugartypes in
    object(self : 'self_type)
      inherit SugarTraversals.map as super

      val next = ref 0
      val imports   : Position.t StringTrie.t ref = ref StringTrie.empty
      val aliens    : int StringMap.t ref = ref StringMap.empty (* TODO FIXME: track provenance of alien imports. *)
      val redundant : Position.t list StringTrie.t ref = ref StringTrie.empty

      method reset =
        imports := StringTrie.empty;
        redundant := StringTrie.empty;
        aliens := StringMap.empty;
        next := 0
      method add_alien lib =
        if not (StringMap.mem lib !aliens) then
          (incr next;
           aliens := StringMap.add lib !next !aliens)
      method add import pos =
        if StringTrie.mem import !imports
        then try
            let same = StringTrie.find import !redundant in
            redundant := StringTrie.add import (pos :: same) !redundant
            with Not_found -> redundant := StringTrie.add import [pos] !redundant
        else imports := StringTrie.add import pos !imports
      method get_aliens =
        let aliens =
          StringMap.fold
            (fun lib i aliens ->
              (lib, i) :: aliens)
            !aliens []
        in
        List.(map fst (sort (fun (_, i) (_, j) -> Pervasives.compare i j) aliens))
      method get_imports = !imports
      method get_redundant_imports = !redundant

      (* This overloading of [binding] collects imports. *)
      method! binding = function
        | ({ node = Import names; pos } as b) ->
           self#add (normalise names) pos; b
        | ({ node = Foreign (_, _, _, lib, _); _ } as b) ->
           self#add_alien lib; b
        | b -> super#binding b

      method bindings = function
        | [] -> []
        | {node=AlienBlock (lang, lib, decls); pos} :: bs->
           self#add_alien lib;
           let bs' =
             List.map
               (fun (bndr, dt) ->
                 let name = Binder.to_name bndr in
                 (* TODO: make a fresh binder with [name]. *)
                 WithPos.make ~pos (Foreign (bndr, name, lang, lib, dt)))
               decls
           in bs' @ self#bindings bs
        | b :: bs -> self#binding b :: self#bindings bs
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

  let rec load = fun visitor rooted_cursor pos st ->
    let open Compilation_unit in
    let rel_cursor = Cursor.relativise rooted_cursor in
    (* Check whether the compilation unit pointed to by [rcursor] has
       already been loaded... *)
    try
      let comp_unit = Store.ByName.find rel_cursor st.store in
      (* ... it may be in preload state, in which case we got ourselves a cyclic dependency. *)
      if Compilation_unit.is_loaded comp_unit then comp_unit
      else raise (CyclicDependency ("FIXME" , pos))
    with Notfound.NotFound _ ->
      (* ... otherwise continue to process compilation unit induced by [rcursor]. *)
      let comp_unit = Compilation_unit.empty () in
      (* Add the compilation unit to the loader state. *)
      let filename = Cursor.to_filename rooted_cursor in
      st.store <- Store.add filename rel_cursor comp_unit st.store;
      (* Parse the file. *)
      let (ast, pos_ctxt) = P.parse filename in
      (* Get the imports. *)
      ignore(visitor#reset);
      let ast = visitor#program ast in
      let rooted_cursors = resolve (visitor#get_imports) st in
      (* Printf.printf "|rooted_cursors| = %d, |visitor#get_imports| = %d\n%!" (List.length rooted_cursors) (StringTrie.size visitor#get_imports); *)
      let _redundant_imports = visitor#get_redundant_imports in
      let aliens = visitor#get_aliens in
      (* Recursively attempt to load each dependency. *)
      let linkset = load_dependencies visitor rooted_cursors st [] in
      (* Promote the compilation unit. *)
      comp_unit.linkset <- linkset;
      comp_unit.source  <- (ast, pos_ctxt);
      comp_unit.aliens  <- aliens;
      Compilation_unit.promote comp_unit;
      (* TODO FIXME: handle redundant imports. *)
      comp_unit
  and load_dependencies visitor rooted_cursors st linkset =
    match rooted_cursors with
    | [] -> in_source_order linkset
    | (rooted_cursor, pos) :: rooted_cursors ->
       let comp_unit =
         try load visitor rooted_cursor pos st
         with e when is_import_error e ->
           raise (ImportError { source_file = Cursor.to_filename rooted_cursor; position = pos; error = e })
       in
       let linkset = (comp_unit.Compilation_unit.id, pos) :: linkset in
       load_dependencies visitor rooted_cursors st linkset
  and in_source_order linkset =
    (* It is important to preserve the source load order, in order to
       guarantee that any top-level side effects are played in lexical
       order. I expect comparing character positions is sufficient to
       reconstruct the original source order. *)
    let sorted =
      List.sort
        (fun (_, (pos : SourceCode.Position.t)) (_, pos') ->
          let open SourceCode in
          let pos = Position.start pos in
          let pos' = Position.start pos' in
          Pervasives.compare
            pos.Lexing.pos_cnum
            pos'.Lexing.pos_cnum)
        linkset
    in List.map fst sorted

  (* Attempts to (pre)load the compilation unit induced by the given
     file name. As a side effect, it loads the dependencies of the
     compilation unit transitively. *)
  let preload : ?implicit_dependencies:Compilation_unit.t list -> string -> t -> (Compilation_unit.t * t)
    = fun ?(implicit_dependencies=[]) source_file st ->
    let visitor = make_preload_obj () in
    let main_cursor = match Namespace.canonicalise source_file st.index with
      | [] -> failwith ("No such file " ^ source_file)
      | [x] -> Printf.printf "file: %s\n%!" (Cursor.to_filename x); x
      | _   -> failwith ("Multiple matches for " ^ source_file)
    in
    let comp_unit = load visitor main_cursor SourceCode.Position.dummy st in
    let () =
      match implicit_dependencies with
      | [] -> ()
      | deps ->
         let open Compilation_unit in
         let linkset =
           List.fold_right
             (fun other linkset ->
               (* TODO emit internal warning when trying to create a cyclic dependency. *)
               if List.mem comp_unit.id linkset || other.id = comp_unit.id
               then linkset
               else other.id :: linkset)
             deps comp_unit.linkset
         in
         comp_unit.linkset <- linkset
    in
    (comp_unit, st)

  let bootstrap : string -> t -> Compilation_unit.t * t
    = fun source_file st ->
    let main_cursor = Namespace.assume source_file st.index in
    if Sys.file_exists source_file then
      let visitor = make_preload_obj () in
      (load visitor main_cursor SourceCode.Position.dummy st, st)
    else
      let comp_unit = Compilation_unit.make_ready () in
      let filename = Cursor.to_filename main_cursor in
      st.store <- Store.add filename (Cursor.relativise main_cursor) comp_unit st.store;
      (comp_unit, st)

  let compute_load_order : t -> LoadOrder.t
    = fun { store; _ } ->
    let open Compilation_unit in
    let completed = Hashtbl.create 32 in
    let in_progress = Hashtbl.create 32 in
    let rec visit comp_unit lo =
      if Hashtbl.mem completed comp_unit.id then lo
      else if Hashtbl.mem in_progress comp_unit.id then raise (Invalid_argument ("Cyclic load order " ^ (String.concat "." (Store.ById.as_qualified_name comp_unit.id store))))
      else let () = Hashtbl.add in_progress comp_unit.id () in
           let lo =
             List.fold_right
               (fun id lo -> visit (Store.ById.find id store) lo)
               comp_unit.linkset lo
           in
           Hashtbl.add completed comp_unit.id ();
           (* there is no need to waste computation removing
              [comp_unit.id] from [in_progress]. *)
           LoadOrder.add comp_unit.id lo
    in
    Store.ById.fold
      (fun id comp_unit lo ->
        if not Hashtbl.(mem completed id || mem in_progress id)
        then visit comp_unit lo
        else lo)
    store (LoadOrder.empty store)
end
