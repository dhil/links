open Utility
open Getopt
module BS = Basicsettings

module CmdlineArg = struct
  open Getopt
  exception Exit
  module Arg = struct
    type t = Short of char | Long of string
    let is_absent : t -> bool = function
      | Short c -> c = '\000'
      | Long s  -> s = ""

    let to_string : t -> string = function
      | Short c -> Printf.sprintf "%c" c
      | Long s  -> s
  end
  type arg_spec =
    { short: Arg.t;
      long: Arg.t;
      param: [`Flag | `Val of string];
      doc: string; }
    
  type opt' = char * string * ((unit -> unit) option) * (((string -> unit) * string) option) * string
  let help options () =
    let msg =
      "Options:" ^
        (List.fold_left
           (fun acc x -> acc ^ "\n" ^ x) "" options)
    in
    Printf.fprintf stdout "%s\n" msg; flush stdout;
    raise Exit

  let parse : opt' list -> (string -> unit) -> string -> unit
    = fun options anon_fn usage ->
      let options =
        List.map
          (fun ((short, long, action, valhandler, doc) : opt') ->
            match valhandler with
            | Some (valhandler, arg_desc) -> (short, long, arg_desc, action, Some valhandler, doc)
            | None -> (short, long, "", action, None, doc))
          options
      in
      let longest_argname =
        List.fold_left
          (fun res (_,long,arg,_,_,_) ->
            let len = (String.length arg) + (String.length long) in
            if len > res then len
            else res)
          0 options
      in
      let make_doc_string (short, long, arg, doc) =
        let make_whitespace len_longest_argname argname argvalue =
          let len = len_longest_argname - (String.length argname) - (String.length argvalue) in
          (* Printf.fprintf stdout "%d - %s<%s>:%d\n" len_longest_argname argname argvalue len; flush stdout; *)
          (String.make len ' ')
        in
        if short <> noshort && long = nolong then
          let argname = Printf.sprintf "-%c" short in
          let whitespace = make_whitespace (longest_argname+4) "" arg in
          Printf.sprintf "  %s %s %s %s" argname arg whitespace doc
        else if short = noshort && long <> nolong then
          let argname = Printf.sprintf "--%s" long in
          let whitespace = make_whitespace (longest_argname+2) argname arg in
          Printf.sprintf "      %s %s %s %s" argname arg whitespace doc
        else
          let short_argname = Printf.sprintf "-%c" short in
          let long_argname  = Printf.sprintf "--%s" long in
          let whitespace = make_whitespace (longest_argname+2) long_argname arg in
          Printf.sprintf "  %s, %s %s %s %s" short_argname long_argname arg whitespace doc
      in
      let options, docs =
        List.fold_left
          (fun (options, docs) (short, long, arg, action, valhandler, doc) ->
            (short, long, action, valhandler) :: options, (make_doc_string (short, long, arg, doc)) :: docs)
          ([],[]) options
      in
      let help_doc = make_doc_string ('h', "help", "", "Displays this help message") in
      let help = ('h', "help", Some (help (List.rev (help_doc :: docs))), None) in
      parse_cmdline (help :: options) anon_fn
end


let to_evaluate : string list ref = ref []
let to_precompile : string list ref = ref []
let file_list : string list ref = ref []

let set_web_mode() = (
    (* When forcing web mode using the command-line argument, default
     the CGI environment variables to a GET request with no params--
     i.e. start running with the main expression. *)
  if not(is_some(getenv "REQUEST_METHOD")) then
    Unix.putenv "REQUEST_METHOD" "GET";
  if not(is_some(getenv "QUERY_STRING")) then
    Unix.putenv "QUERY_STRING" "";
  Settings.set_value BS.web_mode true
  )

let print_keywords = ref false
let print_cache : (bool * string option) ref = ref (false, None)

let config_file   : string option ref = ref BS.config_file_path
let options : CmdlineArg.opt' list =
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    ('x',     nolong, None, Some (ignore, "BAR"), "Foo");
    ('d',     "debug",               set BS.debugging_enabled true,     None, "Enable debugging information");
    ('w',     "web_mode",            Some set_web_mode,                 None, "Enable web mode");
    (noshort, "optimise",            set BS.optimise true,              None, "Apply code optimisations");
    (noshort, "measure-performance", set BS.Performance.measuring true, None, "Run Links in performance measuring mode");
    ('n',     "no-types",            set BS.printing_types false,       None, "");
    ('e',     "evaluate",            None,                              Some ((fun str -> push_back str to_evaluate), "FILE"), "Evaluate script");
    ('m',     "modules",             set BS.modules true,               None, "Enable modules (experimental extension)");
    (noshort, "dump",                None,                              Some ((fun filename -> print_cache := (true, Some filename)), "FILE"), "Print cache");
    (noshort, "precompile",          None,                              Some ((fun file -> push_back file to_precompile), "FILE"), "Precompile source");
    (noshort, "print-keywords",      Some (fun () -> print_keywords := true), None, "Prints Links keywords and exits");
    (noshort, "pp",                 None,                              Some ((Settings.set_value BS.pp), "FILE"), "Runs a given preprocessor on source programs prior to compilation");
    (noshort, "path",               None,                              Some ((fun str -> Settings.set_value BS.links_file_paths str), "PATH"), "Colon separated list of source directories");
    (noshort, "config",             None,                              Some ((fun name -> config_file := Some name), "FILE"), "Links configuration file");
    (noshort, "enable-handlers",    set BS.Handlers.enabled true,      None, "Enable effect handlers (experimental extension)");
    ('r',     "rlwrap",             set BS.Readline.native_readline false, None, "Disable the built-in read line");
    ]

let _ =
  try
    CmdlineArg.parse options (fun i -> push_back i file_list) "";
    (match !config_file with
    | None -> ()
    | Some file -> Settings.load_file false file);
  with Error msg -> Printf.fprintf stderr "error: %s\n" msg; flush stderr; exit 1
  | CmdlineArg.Exit -> exit 0
