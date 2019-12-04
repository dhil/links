open CommonTypes
open Webserver_types
open Ir
open Lwt
open Utility
open Proc
open Var

let runtime_error msg = Errors.runtime_error msg
let internal_error message =
  Errors.internal_error ~filename:"evalir.ml" ~message

(* Builtins *)
module Builtins = struct
  type t =
    [ Value.t
    | `PDataFun of (RequestData.request_data -> Value.t list -> Value.t)
    | `PFun of (Value.t list -> Value.t) ]

  let apply : t -> RequestData.request_data -> Value.t list -> Value.t
    = fun prim req_data args ->
    match prim with
    | `PDataFun fn -> fn req_data args
    | `PFun fn -> fn args
    | _ -> raise (internal_error "Cannot apply non-function primitive")

  (* Relational operators. *)
  let rec equal l r =
    match l, r with
    | `Bool l  , `Bool r   -> (l : bool)  = r
    | `Int l   , `Int r    -> (l : int)   = r
    | `Float l , `Float r  -> (l : float) = r
    | `Char l  , `Char r   -> (l : char)  = r
    | `String l, `String r -> String.equal l r
    | `Record lfields, `Record rfields ->
       let rec one_equal_all = (fun alls (ref_label, ref_result) ->
           match alls with
           | [] -> false
           | (label, result) :: _ when label = ref_label -> equal result ref_result
           | _ :: alls -> one_equal_all alls (ref_label, ref_result))
       in
       List.for_all (one_equal_all rfields) lfields && List.for_all (one_equal_all lfields) rfields
    | `Variant (llabel, lvalue), `Variant (rlabel, rvalue) ->
       llabel = rlabel && equal lvalue rvalue
    | `List (l), `List (r) -> equal_lists l r
    | l, r ->
       raise (runtime_error
                (Printf.sprintf "Comparing %s with %s which either does not make sense or isn't implemented." (Value.string_of_value l) (Value.string_of_value r)))
  and equal_lists l r =
    match l,r with
    | [], [] -> true
    | (l::ls), (r::rs) -> equal l r && equal_lists ls rs
    | _,_ -> false


  let rec less l r =
    match l, r with
    | `Bool l, `Bool r   -> (l : bool) < r
    | `Int l, `Int r     -> (l : int)  < r
    | `Float l, `Float r -> (l : float) < r
    | `Char l, `Char r   -> (l : char) < r
    | `String l, `String r -> (l : string) < r
    (* Compare fields in lexicographic order of labels *)
    | `Record lf, `Record rf ->
       let order = List.sort (fun x y -> compare (fst x) (fst y)) in
       let lv, rv = List.map snd (order lf), List.map snd (order rf) in
       let rec compare_list = function
         | [] -> false
         | (l,r)::_ when less l r -> true
         | (l,r)::_ when less r l -> false
         | _::rest                -> compare_list rest in
       compare_list (List.combine lv rv)
    | `List (l), `List (r) -> less_lists (l,r)
    | l, r ->  raise (runtime_error ("Cannot yet compare "^ Value.string_of_value l ^" with "^ Value.string_of_value r))
  and less_lists = function
    | _, [] -> false
    | [], (_::_) -> true
    | (l::_), (r::_) when less l r -> true
    | (l::_), (r::_) when less r l -> false
    | (_::l), (_::r) -> less_lists (l, r)

  let less_or_equal l r = less l r || equal l r

  let add_attribute : Value.t * Value.t -> Value.t -> Value.t =
    let rec filter = fun name ->
      function
      | [] -> []
      | Value.Attr (s, _) :: nodes when s = name -> filter name nodes
      | Value.NsAttr (ns, s, _) :: nodes when ns ^ ":" ^ s = name -> filter name nodes
      | node :: nodes -> node :: filter name nodes
    in
    fun (name,value) ->
    let name = Value.unbox_string name
    and value = Value.unbox_string value
    in let new_attr = match String.split_on_char ':' name with
         | [ n ] -> Value.Attr(n, value)
         | [ ns; n ] -> Value.NsAttr(ns, n, value)
         | _ -> raise (runtime_error ("Attribute-name con only contain one colon for namespacing. Multiple found: " ^ name))
       in function
       | `XML (Value.Node (tag, children))       -> `XML (Value.Node (tag, new_attr :: filter name children))
       | `XML (Value.NsNode (ns, tag, children)) -> `XML (Value.NsNode (ns, tag, new_attr :: filter name children))
       | r -> raise (runtime_error ("cannot add attribute to " ^ Value.string_of_value r))

 let add_attributes : (Value.t * Value.t) list -> Value.t -> Value.t =
   List.fold_right add_attribute

  let builtins : (string, t) Hashtbl.t =
    let binop impl unbox box = function
      | [x; y] -> box (impl (unbox x) (unbox y))
      | _ -> raise (internal_error "arity error in binary operation")
    in
    let binop' impl box = function
      | [x; y] -> box (impl x y)
      | _ -> raise (internal_error "arity error in binary operation")
    in
    let int_op impl =
      binop impl Value.unbox_int Value.box_int
    in
    let float_op impl =
      binop impl Value.unbox_float Value.box_float
    in
    let unary impl unbox box = function
      | [x] -> box (impl (unbox x))
      | _ -> raise (internal_error "arity error in unary operation")
    in
    let binary impl unboxl unboxr box = function
      | [x; y] -> box (impl (unboxl x) (unboxr y))
      | _ -> raise (internal_error "arity error in binary operation")
    in
    let ternary impl unbox1 unbox2 unbox3 box = function
      | [x; y; z] -> box (impl (unbox1 x) (unbox2 y) (unbox3 z))
      | _ -> raise (internal_error "arity error in ternary operation")
    in
    let nullary impl box = function
      | [] -> box (impl ())
      | _ -> raise (internal_error "arity error in nullary operation")
    in
    let float_fn impl =
      unary impl Value.unbox_float Value.box_float
    in
    let string_to_xml : Value.t -> Value.t = function
      | `String s -> `List [`XML (Value.Text s)]
      | _ -> raise (runtime_error "non-string value passed to xml conversion routine")
    in
    let string_of_int'
      = unary string_of_int Value.unbox_int Value.box_string
    in
    let string_of_float''
      = unary string_of_float' Value.unbox_float Value.box_string
    in
    let not_implemented name
      = unary (fun _ -> raise (runtime_error (Printf.sprintf "The %s function has not yet been implemented on the server." name))) (fun _ -> assert false) (fun x -> x )
    in
    let id x = x in
    let implementations =
      [| (* Boolean operations. *)
        "%bool_not"  , `PFun (unary not Value.unbox_bool Value.box_bool)
        (* Integer operations. *)
      ; "%int_plus"  , `PFun (int_op (+))
      ; "%int_minus" , `PFun (int_op (-))
      ; "%int_mult"  , `PFun (int_op ( * ))
      ; "%int_div"   , `PFun (int_op (/))
      ; "%int_mod"   , `PFun (int_op (mod))
      ; "%int_negate", `PFun (unary (~-) Value.unbox_int Value.box_int)
      (* Floating point operations. *)
      ; "%float_plus"  , `PFun (float_op (+.))
      ; "%float_minus" , `PFun (float_op (-.))
      ; "%float_mult"  , `PFun (float_op ( *. ))
      ; "%float_div"   , `PFun (float_op (/.))
      ; "%float_negate", `PFun (float_fn (~-.))
      (* Trig functions. *)
      ; "%float_floor"  , `PFun (float_fn floor)
      ; "%float_ceiling", `PFun (float_fn ceil)
      ; "%float_cos"    , `PFun (float_fn cos)
      ; "%float_sin"    , `PFun (float_fn sin)
      ; "%float_tan"    , `PFun (float_fn tan)
      ; "%float_log"    , `PFun (float_fn log)
      ; "%float_log10"  , `PFun (float_fn log10)
      ; "%float_sqrt"   , `PFun (float_fn sqrt)
      (* String operations. *)
      ; "%str_cat", `PFun (binop (^) Value.unbox_string Value.box_string)
      (* Relational operators. *)
      ; "%rel_poly_eq"    , `PFun (binop' equal Value.box_bool)
      ; "%rel_poly_neq"   , `PFun (binop' (fun x y -> not (equal x y)) Value.box_bool)
      ; "%rel_poly_lt"    , `PFun (binop' less Value.box_bool)
      ; "%rel_poly_gt"    , `PFun (binop' (flip less) Value.box_bool)
      ; "%rel_poly_le"    , `PFun (binop' less_or_equal Value.box_bool)
      ; "%rel_poly_ge"    , `PFun (binop' (flip less_or_equal) Value.box_bool)
      (* Conversion functions *)
      ; "%conv_int_to_string", `PFun string_of_int'
      ; "%conv_int_to_float" , `PFun (unary float_of_int Value.unbox_int Value.box_float)
      ; "%conv_int_to_xml"   , `PFun (string_of_int' ->- string_to_xml)

      ; "%conv_float_to_int"    , `PFun (unary int_of_float Value.unbox_float Value.box_int)
      ; "%conv_float_to_string" , `PFun string_of_float''
      ; "%conv_float_to_xml"    , `PFun (string_of_float'' ->- string_to_xml)

      ; "%conv_string_to_int"   , `PFun (unary int_of_string Value.unbox_string Value.box_int)
      ; "%conv_string_to_float" , `PFun (unary float_of_string Value.unbox_string Value.box_float)
      ; "%conv_string_to_xml"   , `PFun (unary (fun s -> [`XML (Value.Text s)]) Value.unbox_string Value.box_list)

      (* System primitives. *)
      ; "%exit"   , `Continuation Value.Continuation.empty
      ; "%sysexit", `PFun (unary exit Value.unbox_int (fun _ -> assert false))
      ; "%show"   , `PFun (unary Value.string_of_value id Value.box_string)

      (* Process operations. *)
      (* The functions: Send, recv
         spawn{At,Client,Angel,AngelAt,Wait,Wait'} are currently
         implemented by the interpreter. *)
      ; "%proc_self"     , `PFun (nullary Proc.get_current_pid (fun pid -> Value.box_pid (`ServerPid pid)))
      ; "%proc_here"     , `PFun (nullary (fun () -> `ServerSpawnLoc) Value.box_spawn_loc)
      ; "%proc_there"    , `PDataFun (fun req_data _ ->
                               let client_id = RequestData.get_client_id req_data in
                               Value.box_spawn_loc (`ClientSpawnLoc client_id))
      ; "%proc_have_mail", `PFun (not_implemented "haveMail")
      ; "%proc_sleep"    , `PFun (not_implemented "sleep")
      (* Session functions are implemented by the interpreter. *)
      (* Access point functions are implemented by the interpreter. *)
      (* List operations. *)
      ; "%list_nil"   , `List []
      ; "%list_cons"  , `PFun (binary (fun x xs -> x :: xs) id Value.unbox_list Value.box_list)
      ; "%list_concat", `PFun (binop (@) Value.unbox_list Value.box_list)
      ; "%list_hd"    , `PFun (unary List.hd Value.unbox_list id)
      ; "%list_tl"    , `PFun (unary List.tl Value.unbox_list Value.box_list)
      ; "%list_length", `PFun (unary List.length Value.unbox_list Value.box_int)
      ; "%list_take"  , `PFun (binary ListUtils.take Value.unbox_int Value.unbox_list Value.box_list)
      ; "%list_drop"  , `PFun (binary ListUtils.drop Value.unbox_int Value.unbox_list Value.box_list)
      ; "%list_max"   , `PFun (unary (function
                                   | [] ->
                                      `Variant ("None", `Record [])
                                   | x :: xs ->
                                      `Variant ("Some", List.fold_left
                                                          (fun x y -> if less x y then y else x) x xs))
                                 Value.unbox_list id)
      ; "%list_min"   , `PFun (unary (function
                                   | [] ->
                                      `Variant ("None", `Record [])
                                   | x :: xs ->
                                      `Variant ("Some", List.fold_left
                                                          (fun x y -> if less x y then x else y) x xs))
                                 Value.unbox_list id)
      (* XML operations. *)
      ; "%xml_child_nodes", `PFun (unary (function
                                         | [`XML (Value.Node (_, children))] ->
                                            List.filter (function
                                                | Value.Node _ -> true
                                                | Value.NsNode _ -> true
                                                | _ -> false)
                                              children
                                         | [ `XML (Value.NsNode (_, _, children)) ] ->
                                            List.filter (function
                                                | Value.Node _ -> true
                                                | Value.NsNode _ -> true
                                                | _ -> false)
                                              children
                                         | _ -> raise (internal_error "non-XML given to childNodes")) Value.unbox_list (fun xs -> Value.box_list (List.map Value.box_xml xs)))
      ; "%xml_add_attributes", `PFun (binary (fun xmlitems attrs ->
                                          let attrs =
                                            List.map (fun p -> Value.unbox_pair p) attrs
                                          in
                                          List.map (add_attributes attrs) xmlitems)
                                    Value.unbox_list Value.unbox_list Value.box_list)
      ; "%xml_attribute", `PFun (binary (fun elem attr ->
                                     let find_attr cs =
                                       (try match List.find (function
                                                      | Value.Attr (k, _) ->
                                                         String.equal k attr
                                                      | Value.NsAttr (ns, k, _) ->
                                                         String.equal (Printf.sprintf "%s:%s" ns k) attr
                                                      | _ -> false
                                                    ) cs with
                                            | Value.Attr (_, v) -> `Variant ("Just", Value.box_string v)
                                            | Value.NsAttr (_, _, v) -> `Variant ("Just", Value.box_string v)
                                            | _ -> raise (internal_error "Incorrect arguments to `attribute' function")
                                        with NotFound _ -> `Variant ("Nothing", `Record []))
                                     in
                                     match elem with
                                     | [`XML (Value.Node (_, children))] -> find_attr children
                                     | [`XML (Value.NsNode (_, _, children))] -> find_attr children
                                     | _ -> raise (internal_error  "Non-element node given to attribute function"))
                                   Value.unbox_list Value.unbox_string id)
      ; "%xml_make", `PFun (ternary (fun name attrs children ->
                                let attrs' =
                                  List.map (function
                                      | `Record [ ("1", key); ("2", value) ] ->
                                         Value.Attr (Value.unbox_string(key), Value.unbox_string(value))
                                      | _ -> raise (internal_error "non-XML in makeXml"))
                                    attrs
                                in
                                Value.Node
                                  (name, List.map (function
                                             | (`XML x) -> x
                                             | _ -> raise (internal_error "non-XML in makeXml")
                                           ) children @ attrs'))
                              Value.unbox_string Value.unbox_list Value.unbox_list Value.box_xml)
      ; "%xml_to_variant", `PFun (unary (fun xs ->
                                      List.map (function
                                          | `XML x -> Value.value_of_xmlitem x
                                          | _ -> raise (internal_error "non-XML passed to xmlToVariant")) xs) Value.unbox_list Value.box_list)
      ; "%xml_variant_to_xml", `PFun (unary (Value.xml_of_variants ->- (List.map Value.box_xml)) id Value.box_list)
      ; "%xml_item_to_variant", `PFun (unary Value.value_of_xmlitem Value.unbox_xml id)
      ; "%xml_variant_to_xml_item", `PFun (unary Value.xmlitem_of_variant id Value.box_xml)
      ; "%xml_get_tag_name", `PFun (unary (function
                                        | [ `XML (Value.Node (name, _)) ] | [ `XML (Value.NsNode (_, name, _)) ] -> name
                                        | _ -> raise (internal_error "non-element passed to getTagName"))
                                      Value.unbox_list Value.box_string)
      ; "%xml_get_namespace", `PFun (unary (function
                                         | [ `XML (Value.Node (_, _)) ] -> ""
                                         | [ `XML (Value.NsNode (ns, _, _)) ] -> ns
                                         | _ -> raise (internal_error "non-element passed to getNamespace")) Value.unbox_list Value.box_string)
      ; "%xml_get_attributes", `PFun (unary (fun v ->
                                          let attr_to_record = function
                                            | (Value.Attr (name, value)) -> `Record [("1", Value.box_string name); ("2", Value.box_string value)]
                                            | (Value.NsAttr (ns, name, value)) -> `Record [ ("1", Value.box_string (ns ^ ":" ^ name)); ("2", Value.box_string value) ]
                                            | _ -> assert false
                                          and is_attr = function
                                            | Value.Attr _ -> true
                                            | Value.NsAttr _ -> true
                                            | _ -> false
                                          in match v with
                                             | [ `XML (Value.Node (_, children)) ]      -> List.(map attr_to_record (filter is_attr children))
                                             | [ `XML (Value.NsNode (_, _, children)) ] -> List.(map attr_to_record (filter is_attr children))
                                             | _ -> raise (internal_error "non-element given to getAttributes")) Value.unbox_list Value.box_list)
      ; "%xml_get_child_nodes", `PFun (unary (fun v ->
                                           let is_xml_node = function
                                             | Value.Node _ | Value.NsNode _ -> true
                                             | _ -> false
                                           in
                                           match v with
                                           | [ `XML (Value.Node(_, children)) ] ->
                                              List.filter is_xml_node children
                                           | [ `XML (Value.NsNode(_, _, children)) ] ->
                                              List.filter is_xml_node children
                                           | _ -> raise (internal_error "non-element given to getChildNodes")) Value.unbox_list ((List.map Value.box_xml) ->- Value.box_list))
      ; "%xml_parse", `PFun (unary ParseXml.parse_xml Value.unbox_string (Value.box_xml ->- (fun v -> Value.box_list [v])))
      (* Server time. *)
      ; "%server_time", `PFun (nullary (Unix.time ->- int_of_float) Value.box_int)
      ; "%server_time_ms", `PFun (nullary Utility.time_milliseconds Value.box_int)
      (* Time. *)
      ; "%date_to_int", `PFun (unary (fun r ->
                                   let lookup s =
                                     Value.unbox_int (List.assoc s r) in
                                   let tm = {
                                       Unix.tm_sec = lookup "seconds";
                                       Unix.tm_min = lookup "minutes";
                                       Unix.tm_hour = lookup "hours";
                                       Unix.tm_mday = lookup "day";
                                       Unix.tm_mon = lookup "month";
                                       Unix.tm_year = (lookup "year" - 1900);
                                       Unix.tm_wday = 0; (* ignored *)
                                       Unix.tm_yday =  0; (* ignored *)
                                       Unix.tm_isdst = false}
                                   in
                                   let t, _ = Unix.mktime tm in
                                   int_of_float t) Value.unbox_record Value.box_int)
      ; "%date_int_to_date", `PFun (unary (fun t ->
                                        let tm = Unix.localtime (float_of_int t) in
                                        [ "year", Value.box_int (tm.Unix.tm_year + 1900);
                                          "month", Value.box_int tm.Unix.tm_mon;
                                          "day", Value.box_int tm.Unix.tm_mday;
                                          "hours", Value.box_int tm.Unix.tm_hour;
                                          "minutes", Value.box_int tm.Unix.tm_min;
                                          "seconds", Value.box_int tm.Unix.tm_sec ])
                                      Value.unbox_int Value.box_record)
                              (* HTTP manipulation. *)
      ; "%http_redirect", `PDataFun (fun req_data args ->
                              let url =
                                match args with
                                | [url] -> Value.unbox_string url
                                | _ -> raise (internal_error "arity mismatch in redirect")
                              in
                              (* This is all quite hackish, just testing an idea. --ez *)
                              let resp_headers = RequestData.get_http_response_headers req_data in
                              RequestData.set_http_response_headers req_data (("Location", url) :: resp_headers);
                              RequestData.set_http_response_code req_data 302;
                              Value.unit)
                           (* Database. *)
      ; "%db_get_config", `PFun (nullary (fun () ->
                                     let args = Utility.from_option "" (Settings.get Database.connection_info) in
                                     match Settings.get DatabaseDriver.driver with
                                     | None ->
                                        raise (Errors.settings_error "Default database driver not defined. Set `database_driver`.")
                                     | Some driver ->
                                        [ "driver", Value.box_string driver;
                                          "args", Value.box_string args ])
                                   Value.box_record)
      (* Character functions. *)
      ; "%char_ord", `PFun (unary Char.code Value.unbox_char Value.box_int)
      ; "%char_chr", `PFun (unary Char.chr Value.unbox_int Value.box_char)
      (* Regexes. *)
      ; "%regex_tilde", `PFun (binary (fun s r ->
                                   let regex = Regex.compile_ocaml r in
                                   Str.string_match regex s 0)
                                 Value.unbox_string Linksregex.Regex.ofLinks Value.box_bool)
      ; "%regex_ltilde", `PFun (binary (fun string (re, ngroups) ->
                                    let regex = Regex.compile_ocaml re in
                                    if not (Str.string_match regex string 0)
                                    then []
                                    else let rec accumMatches : Value.t list -> int -> Value.t list
                                           = fun l i ->
                                           if i = 0
                                           then (Value.box_string (Str.matched_group 0 string) :: l)
                                           else try let grp = Value.box_string (Str.matched_group i string) in
                                                    accumMatches (grp :: l) (i - 1)
                                                with
                                                  NotFound _ -> accumMatches (Value.empty_string :: l) (i - 1)
                                         in
                                         accumMatches [] ngroups)
                                  Value.unbox_string Linksregex.Regex.ofLinksNGroups Value.box_list)
      ; "%regex_stilde", `PFun (binary (fun s r ->
                                    let open Regex in
                                    match r with
                                    | Replace (l, t) ->
                                       let regex = Regex.compile_ocaml l in
                                       Utility.decode_escapes (Str.replace_first regex t s)
                                    | _ -> assert false)
                                  Value.unbox_string Linksregex.Regex.ofLinks Value.box_string)
      (* Strings. *)
      ; "%str_char_at", `PFun (binary (fun s i ->
                                   try s.[i] with
                                   | Invalid_argument _ -> raise (runtime_error "charAt: invalid index"))
                                 Value.unbox_string Value.unbox_int Value.box_char)
      ; "%str_sub", `PFun (ternary (fun s start len ->
                               try String.sub s start len with
                               | Invalid_argument _ -> raise (runtime_error "strsub: invalid arguments"))
                             Value.unbox_string Value.unbox_int Value.unbox_int Value.box_string)
      ; "%str_len", `PFun (unary String.length Value.unbox_string Value.box_int)
      ; "%str_escape", `PFun (unary String.escaped Value.unbox_string Value.box_string)
      ; "%str_unescape", `PFun (unary Scanf.unescaped Value.unbox_string Value.box_string)
      ; "%str_contains", `PFun (binary String.contains Value.unbox_string Value.unbox_char Value.box_bool)
      ; "%str_implode", `PFun (unary StringUtils.implode (Value.unbox_list ->- (List.map Value.unbox_char)) Value.box_string)
      ; "%str_explode", `PFun (unary StringUtils.explode Value.unbox_string ((List.map Value.box_char) ->- Value.box_list))
      (* Unsafe. *)
      ; "%unsafe_pickle_cont", `PFun (unary Serialisation.MarshalSerialiser.Value.save id Value.box_string)
      ; "%unsafe_cast", `PFun (unary id id id)
                         (* Misc. *)
      ; "%random", `PFun (nullary (fun () -> Random.float 1.0) Value.box_float)
      ; "%environment", `PDataFun (fun req_data _ ->
                            let cgi_params = RequestData.get_cgi_parameters req_data in
                            let makestrpair (x1, x2) =
                              Value.box_record [ ("1", Value.box_string x1); ("2", Value.box_string x2) ]
                            in
                            let is_internal s = Str.string_match (Str.regexp "^_") s 0 in
                            Value.box_list (List.map makestrpair (List.filter (not -<- is_internal -<- fst) cgi_params)))
      ; "%debug", `PFun (unary (fun s -> Debug.print s; []) Value.unbox_string Value.box_record)
      ; "%print", `PFun (unary (fun s -> print_string s; flush stdout; []) Value.unbox_string Value.box_record)
      ; "%javascript", `Bool false
      ; "%error", `PFun (unary (fun s -> raise (runtime_error s)) Value.unbox_string (fun _ -> assert false))
      ; "%gensym", (let next = ref (-1) in
                    `PFun (nullary (fun () -> incr next; !next) Value.box_int))
      (* Sockets. *)
      ; "%socket_connect", `PFun (binary (fun server port ->
                                      try
                                        let server_addr =
                                          try  Unix.inet_addr_of_string server
                                          with Failure _ ->
                                            (Unix.gethostbyname server).Unix.h_addr_list.(0)
                                        in
                                        let sockaddr = Unix.ADDR_INET(server_addr, port) in
                                        let domain = Unix.domain_of_sockaddr sockaddr in
                                        let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
                                        Unix.connect sock sockaddr;
                                        Unix.set_nonblock sock;
                                        `Variant ("Just", Value.box_socket (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock))
                                      with _ -> `Variant ("Nothing", `Record []))
                                    Value.unbox_string Value.unbox_int id)
      ; "%socket_write", `PFun (binary (fun message (_, outc) -> output_string outc message; flush outc; []) Value.unbox_string Value.unbox_socket Value.box_record)
      ; "%socket_read", `PFun (unary (fun (inc, _) ->
                                  try let r = input_line inc in
                                      `Variant ("Just", Value.box_string r)
                                  with Sys_blocked_io ->
                                    `Variant ("Nothing", `Record []))
                                Value.unbox_socket id)
      ; "%socket_close", `PFun (unary (fun (inc, _) -> Unix.shutdown (Unix.descr_of_in_channel inc) Unix.SHUTDOWN_SEND; []) Value.unbox_socket Value.box_record)
      ; "%crypt_hash", `PFun (unary (fun s -> Bcrypt.(string_of_hash (hash s))) Value.unbox_string Value.box_string)
      ; "%crypt_verify", `PFun (binary (fun s s' -> Bcrypt.verify s (Bcrypt.hash_of_string s')) Value.unbox_string Value.unbox_string Value.box_bool)
      (* CLI. *)
      ; "%cli_get_args", `PFun (nullary Settings.get_rest_arguments ((List.map Value.box_string) ->- Value.box_list))
      |]
    in
    let tbl = Hashtbl.create (Array.length implementations) in
    Array.iter
      (fun (prim_name, impl) ->
        Hashtbl.add tbl prim_name impl)
      implementations;
    tbl

  let find name = Hashtbl.find builtins name
  let find_value name =
    match Hashtbl.find builtins name with
    | #Value.t as v -> v
    | _ -> raise (internal_error (Printf.sprintf "%s is not a primitive value." name))
end

let lookup_fun = Tables.lookup Tables.fun_defs
let find_fun = Tables.find Tables.fun_defs

let dynamic_static_routes
  = Settings.(flag "dynamic_static_routes"
              |> convert parse_bool
              |> sync)
let allow_static_routes = ref true


module type EVALUATOR = sig
  type v = Value.t
  type result = Proc.thread_result Lwt.t

  val reify : Value.resumption -> v
  val error : string -> 'a
  val computation : Value.env -> Value.continuation -> Ir.computation -> result
  val finish : Value.env -> v -> result

  val apply : Value.continuation -> Value.env -> v * v list -> result
  val apply_cont : Value.continuation -> Value.env -> v -> result
  val run_program : Value.env -> Ir.program -> (Value.env * v)
end

module Exceptions = struct
  exception EvaluationError of string
  exception Wrong
end

module Evaluator = functor (ContEval : Value.CONTINUATION_EVALUATOR with type v = Value.t
                                                                    and type result = Proc.thread_result Lwt.t
                                                                    and type 'v t := 'v Value.Continuation.t
                                                                    and type 'v resumption := 'v Value.Continuation.resumption)
                   (Webs : WEBSERVER) ->
struct
  type v = Value.t
  type result = Proc.thread_result Lwt.t
  type continuation = Value.continuation
  type resumption = Value.resumption

  module K = struct
    include Value.Continuation
    module Eval = ContEval
  end

  let error msg : 'a = raise (Exceptions.EvaluationError msg)

  let eval_error fmt : 'r =
    Printf.ksprintf error fmt

  let type_error ~action expected value =
    eval_error "Attempting to %s %s (need %s instead)" action (Value.string_of_value value) expected

  let db_connect : Value.t -> Value.database * string = fun db ->
    let driver = Value.unbox_string (Value.project "driver" db) in
    let name = Value.unbox_string (Value.project "name" db) in
    let args = Value.unbox_string (Value.project "args" db) in
    let params =
      if String.equal args "" then name
      else Printf.sprintf "%s:%s" name args
    in
    Value.db_connect driver params

  let lookup_fun_def f =
    match lookup_fun f with
    | None -> None
    | Some (_finfo, _, None, _) ->
       Some (`FunctionPtr (f, None))
      (* begin
       *   match location with
       *   | Location.Server | Location.Unknown ->
       *     (\* TODO: perhaps we should actually use env here - and make
       *        sure we only call this function when it is sufficiently
       *        small *\)
       *     Some (`FunctionPtr (f, None))
       *   | Location.Client ->
       *      let name = Js.var_name_binder (f, finfo) in
       *      Some (`PrimitiveFunction (Value.Primitive.make ~user_friendly_name:name ~object_name:name ()))
       *   | Location.Native -> assert false
       * end *)
    | _ -> assert false

  let find_fun_def f =
    val_of (lookup_fun_def f)

  (* TODO: explicitly distinguish functions and variables in the
     IR so we don't have to do this check every time we look up a
     variable *)
  let lookup_var var env =
    (* if Lib.is_primitive_var var then Lib.primitive_stub_by_code var
     * else *)
      match lookup_fun_def var with
      | None ->
        Value.Env.find var env
      | Some v -> v

   let serialize_call_to_client req_data ((continuation : continuation), name, args) =
     let open Json in
     let client_id = RequestData.get_client_id req_data in
     let conn_url =
       if (Webs.is_accepting_websocket_requests ()) then
         Some (Webs.get_websocket_url ())
       else
         None
     in
     let st = List.fold_left
       (fun st_acc arg -> ResolveJsonState.add_value_information arg st_acc)
       (JsonState.empty client_id conn_url) args in
     let st = ResolveJsonState.add_ap_information client_id st in
     let st = ResolveJsonState.add_process_information client_id st in
     let st = ResolveJsonState.add_channel_information client_id st in
     Json.jsonize_call st (Serialisation.MarshalSerialiser.Continuation.save continuation) name args
      |> Json.json_to_string

   let client_call :
     RequestData.request_data ->
     string ->
     Value.continuation ->
     Value.t list ->
     result =

       fun req_data name cont args ->
         if not(Settings.get Basicsettings.web_mode) then
           raise (Errors.client_call_outside_webmode name);
         Debug.print("Making client call to " ^ name);
  (*        Debug.print("Call package: "^serialize_call_to_client (cont, name, args)); *)
         let call_package =
           Utility.base64encode @@
             serialize_call_to_client req_data (cont, name, args) in
         Proc.abort ("text/plain", call_package)

    let handle_session_exception raise_env frames =
      let affected_channels =
        ChannelVarUtils.affected_channels raise_env frames in
      (* List.iter (fun c -> Printf.printf "%s\n" (Value.string_of_value c)) affected_channels *)
      List.fold_left (fun acc v -> acc >>= (fun _ -> Value.unbox_channel v |> Session.cancel))
        (Lwt.return ())
        affected_channels

  (** {0 Evaluation} *)
  let rec value env : Ir.value -> Value.t Lwt.t = fun v ->
    let constant = function
      | Constant.Bool   b -> `Bool b
      | Constant.Int    n -> `Int n
      | Constant.Char   c -> `Char c
      | Constant.String s -> Value.box_string s
      | Constant.Float  f -> `Float f in

    match v with
    | Constant c -> Lwt.return (constant c)
    | Variable var -> Lwt.return (lookup_var var env)
    | Extend (fields, r) ->
        begin
          opt_app (value env) (Lwt.return (`Record [])) r >>= fun res ->
          match res with
            | `Record fs ->
                let fields = StringMap.bindings fields in
                LwtHelpers.foldr_lwt
                   (fun (label, v) (fs: (string * Value.t) list)  ->
                      if List.mem_assoc label fs then
                        eval_error
                          "Error adding fields: label %s already present" label
                      else
                        value env v >>= fun v ->
                        Lwt.return ((label, v)::fs))
                   fields
                   (Lwt.return []) >>= fun res ->
                Lwt.return (`Record (res @ fs))
            | v -> type_error ~action:"add field to" "record" v
        end
    | Project (label, r) ->
        value env r >>= fun v ->
        begin
          match v with
            | `Record fields when List.mem_assoc label fields ->
                Lwt.return (List.assoc label fields)
            | v -> type_error ~action:("projecting label " ^ label) "record" v
        end
    | Erase (labels, r) ->
        value env r >>= fun v ->
        begin
          match v with
            | `Record fields when
                StringSet.for_all (fun label -> List.mem_assoc label fields) labels ->
                  Lwt.return (
                `Record (StringSet.fold (fun label fields -> List.remove_assoc label fields) labels fields))
            | v ->
               type_error ~action:(Printf.sprintf "erase labels {%s}" (String.concat "," (StringSet.elements labels)))
                 "record" v
        end
    | Inject (label, v, _) ->
        value env v >>= fun v -> Lwt.return (`Variant (label, v))
    | TAbs (_, v) -> value env v
    | TApp (v, _) -> value env v
    | XmlNode (tag, attrs, children) ->
          LwtHelpers.foldr_lwt
            (fun v children ->
              value env v >>= fun v ->
               Lwt.return (List.map Value.unbox_xml (Value.unbox_list v) @ children))
            children (Lwt.return []) >>= fun children ->
          let attrs = StringMap.bindings attrs in
          LwtHelpers.foldr_lwt
            (fun (name, v) attrs ->
               value env v >>= fun str ->
               Lwt.return (Value.Attr (name, Value.unbox_string str) :: attrs))
            (List.rev attrs) (Lwt.return children) >>= fun children ->
          Lwt.return (Value.box_list [Value.box_xml (Value.Node (tag, children))])
    | ApplyPure (f, args) ->
      value env f >>= fun f ->
      (LwtHelpers.sequence (List.map (value env) args)) >>= fun args ->
      Proc.atomically (fun () -> apply K.empty env (f, args))
    | Closure (f, _, v) ->
      value env v >>= fun v ->
      Lwt.return (`FunctionPtr (f, Some v))
    | Coerce (v, _) -> value env v
  and apply_access_point (cont : continuation) env : Value.spawn_location -> result = function
      | `ClientSpawnLoc cid ->
          let apid = Session.new_client_access_point cid in
          apply_cont cont env (`AccessPointID (`ClientAccessPoint (cid, apid)))
      | `ServerSpawnLoc ->
          let apid = Session.new_server_access_point () in
          apply_cont cont env (`AccessPointID (`ServerAccessPoint apid))
  and apply (cont : continuation) env : Value.t * Value.t list -> result =
    let invoke_session_exception () =
      special env cont (DoOperation (Value.session_exception_operation, [], `Not_typed))
    in function
    | `FunctionPtr (f, fvs), args ->
       let (_finfo, (xs, body), z, location) = find_fun f in
       let env =
         match z, fvs with
         | None, None -> env
         | Some z, Some fvs ->
            Value.Env.bind z (fvs, Scope.Local) env
         | None, Some _ -> raise (internal_error "missing binder for closure environment")
         | Some _, None -> raise (internal_error "missing closure environment")
       in
       if Location.is_client location
       then client_call (Value.Env.request_data env) (string_of_int f) cont args
       else let env =
              (* extend env with arguments *)
              List.fold_right2
                (fun x p -> Value.Env.bind x (p, Scope.Local))
                xs args env
            in
            computation_yielding env cont body
    | `PrimitiveFunction desc, args ->
       let fvar = Value.Primitive.var desc in
       begin match (Value.Primitive.object_name desc), args with
       | "%register_event_handlers", [hs] ->
          let key = EventHandlers.register hs in
          apply_cont cont env (`String (string_of_int key))
       (* start of mailbox stuff *)
       | "%proc_send", [pid; msg] ->
          let unboxed_pid = Value.unbox_pid pid in
          (try
             match unboxed_pid with
             (* Send a message to a process which lives on the server *)
             | `ServerPid serv_pid ->
                Lwt.return @@ Mailbox.send_server_message msg serv_pid
             (* Send a message to a process which lives on another client *)
             | `ClientPid (client_id, process_id) ->
                Mailbox.send_client_message msg client_id process_id
           with
             UnknownProcessID id ->
             Debug.print (
                 "Couldn't deliver message because destination process " ^
                   (ProcessTypes.ProcessID.to_string id) ^ " has no mailbox.");
             Lwt.return ()) >>= (fun _ ->
            apply_cont cont env Value.unit)
       | "%proc_spawn_at", [loc; func] ->
          begin match loc with
          | `SpawnLocation (`ClientSpawnLoc client_id) ->
             Proc.create_client_process client_id func >>= fun new_pid ->
             apply_cont cont env (`Pid (`ClientPid (client_id, new_pid)))
          | `SpawnLocation `ServerSpawnLoc ->
             let var = Var.dummy_var in
             let frame = K.Frame.make Scope.Local var Value.Env.empty ([], Ir.apply_fn var []) in
             Proc.create_process false
               (fun () -> apply_cont K.(frame &> empty) env func) >>= fun new_pid ->
             apply_cont cont env (`Pid (`ServerPid new_pid))
          | _ -> assert false
          end
       | "%proc_spawn_angel_at", [loc; func] ->
          begin match loc with
          | `SpawnLocation (`ClientSpawnLoc client_id) ->
             Proc.create_client_process client_id func >>= fun new_pid ->
             apply_cont cont env (`Pid (`ClientPid (client_id, new_pid)))
          | `SpawnLocation (`ServerSpawnLoc) ->
             let var = Var.dummy_var in
             let frame = K.Frame.make Scope.Local var Value.Env.empty ([], Apply (Variable var, [])) in
             Proc.create_process true
               (fun () -> apply_cont K.(frame &> empty) env func) >>= fun new_pid ->
             apply_cont cont env (`Pid (`ServerPid new_pid))
          | _ -> assert false
          end
       | "%proc_spawn_wait", [func] ->
          let our_pid = Proc.get_current_pid () in
          (* Create the new process *)
          let var = Var.dummy_var in
          let frame = K.Frame.make Scope.Local var Value.Env.empty ([], Apply (Variable var, [])) in
          Proc.create_spawnwait_process our_pid
            (fun () -> apply_cont K.(frame &> empty) env func) >>= fun child_pid ->
          (* Now, we need to block this process until the spawned
             process has evaluated to a value.  The idea here is that
             we have a second function, spawnWait', which grabs the
             result from proc.ml. *)
          let fresh_var = Var.fresh_raw_var () in
          let extended_env =
            Value.Env.bind fresh_var (Value.box_pid (`ServerPid child_pid), Scope.Local) env in
          let grab_frame =
            K.Frame.of_expr extended_env
              (Ir.apply_fn fvar [Variable fresh_var]) (* TODO FIXME use a persistent identifier. *)
          in
          (* Now, check to see whether we already have the result; if
             so, we can grab and continue. Otherwise, we need to
             block. *)
          begin
            match Proc.get_spawnwait_result child_pid with
            | Some v -> apply_cont cont env v
            | None ->
               Proc.block (fun () -> apply_cont K.(grab_frame &> cont) env Value.unit)
          end
       | "%proc_spawn_wait'", [child_pid] ->
          let unboxed_pid = Value.unbox_pid child_pid in
          begin
            match unboxed_pid with
            | `ServerPid server_pid ->
               let v = OptionUtils.val_of @@ Proc.get_spawnwait_result server_pid in
               apply_cont cont env v
            | _ -> assert false
          end
       | "%proc_recv", [] ->
          (* If there are any messages, take the first one and apply
             the continuation to it.  Otherwise, block the process
             (put its continuation in the blocked_processes table) and
             let the scheduler choose a different thread.  *)
          begin match Mailbox.pop_message () with
            Some message ->
             Debug.print("delivered message.");
             apply_cont cont env message
          | None ->
             let recv_frame = K.Frame.of_expr env (Ir.apply_fn fvar []) in (* TODO FIXME use a persistent identifier. *)
             Proc.block (fun () -> apply_cont K.(recv_frame &> cont) env Value.unit)
          end
       (* end of mailbox stuff *)
       (* start of session stuff *)
       | "%ap_new", [] ->
          apply_access_point cont env `ServerSpawnLoc
       | "%ap_new_ap", [loc] ->
          let unboxed_loc = Value.unbox_spawn_loc loc in
          apply_access_point cont env unboxed_loc
       | "%new_client_ap", [] ->
          (* Really this should be desugared properly into "there"... *)
          let client_id = RequestData.get_client_id @@ Value.Env.request_data env in
          apply_access_point cont env (`ClientSpawnLoc client_id)
       | "%new_server_ap", [] ->
          apply_access_point cont env `ServerSpawnLoc
       | "%ap_accept", [ap] ->
          let ap = Value.unbox_access_point ap in
          begin
            match ap with
            | `ClientAccessPoint _ ->
               (* TODO: Work out the semantics of this *)
               raise (internal_error "Cannot *yet* accept on a client AP on the server")
            | `ServerAccessPoint apid ->
               Session.accept apid >>= fun ((_, c) as ch, blocked) ->
               let boxed_channel = Value.box_channel ch in
               Debug.print ("Accepting: " ^ (Value.string_of_value boxed_channel));
               if blocked then
                 (* block my end of the channel *)
                 (Session.block c (Proc.get_current_pid ());
                  Proc.block (fun () -> apply_cont cont env boxed_channel))
               else
                 (* other end will have been unblocked in proc *)
                 apply_cont cont env boxed_channel
          end
       | "%ap_request", [ap] ->
          let ap = Value.unbox_access_point ap in
          begin
            match ap with
            | `ClientAccessPoint _ ->
               (* TODO: Work out the semantics of this *)
               raise (internal_error "Cannot *yet* request from a client-spawned AP on the server")
            | `ServerAccessPoint apid ->
               Session.request apid >>= fun ((_, c) as ch, blocked) ->
               let boxed_channel = Value.box_channel ch in
               if blocked then
                 (* block my end of the channel *)
                 (Session.block c (Proc.get_current_pid ());
                  Proc.block (fun () -> apply_cont cont env boxed_channel))
               else
                 (* Otherwise, other end will have been unblocked in
                    proc.ml, return new channel EP *)
                apply_cont cont env boxed_channel
          end
       | "%session_send", [v; chan] ->
          let open Session in
          Debug.print ("sending: " ^ Value.string_of_value v ^ " to channel: " ^ Value.string_of_value chan);
          let unboxed_chan = Value.unbox_channel chan in
          let outp = Session.send_port unboxed_chan in
          Session.send_from_local v outp >>= fun res ->
          begin
            match res with
            | SendOK -> apply_cont cont env chan
            | SendPartnerCancelled ->
               (* If send fails, we need to cancel all carried channels *)
               let contained_channels = Value.get_contained_channels v in
               List.fold_left
                 (fun acc c -> acc >>= fun _ -> Session.cancel c)
                 (Lwt.return ()) contained_channels >>= fun _ ->
               apply_cont cont env chan
          end
       | "%session_recv", [chan] ->
          begin
            let open Session in
            Debug.print("receiving from channel: " ^ Value.string_of_value chan);
            let unboxed_chan = Value.unbox_channel chan in
            let peer_ep = Session.send_port unboxed_chan in
            let block () =
              (* Here, we have to extend the environment with a fresh
                 variable representing the channel, since we can't
                 create an IR application involving a Value.t (only an
                 Ir.value).  This *should* be safe, but still feels a
                 bit unsatisfactory.  It would be nice to refine this
                 further. *)
              let fresh_var = Var.fresh_raw_var () in
              let extended_env = Value.Env.bind fresh_var (chan, Scope.Local) env in
              let grab_frame = K.Frame.of_expr extended_env (Ir.apply_fn fvar [Variable fresh_var]) in (* TODO FIXME use a persistent identifier. *)
              let inp = (snd unboxed_chan) in
              Session.block inp (Proc.get_current_pid ());
              Proc.block (fun () -> apply_cont K.(grab_frame &> cont) env Value.unit)
            in
            let throw_or_block () =
              if Settings.get (Basicsettings.Sessions.exceptions_enabled) then
                invoke_session_exception ()
              else block () in

            if Session.is_endpoint_cancelled peer_ep then
              throw_or_block ()
            else
              match Session.receive unboxed_chan with
              | ReceiveOK v ->
                 Debug.print ("grabbed: " ^ Value.string_of_value v);
                 apply_cont cont env (Value.box_pair v chan)
              | ReceiveBlocked -> block ()
              | ReceivePartnerCancelled ->
                 Session.cancel unboxed_chan >>= fun _ ->
                 throw_or_block ()
          end
       | "%session_link", [chanl; chanr] ->
          let unblock p =
            match Session.unblock p with
            | Some pid -> (*Debug.print("unblocked: "^string_of_int p); *)
               Proc.awaken pid
            | None     -> () in
          Debug.print ("linking channels: " ^ Value.string_of_value chanl ^ " and: " ^ Value.string_of_value chanr);
          let (out1, in1) = Value.unbox_channel chanl in
          let (out2, in2) = Value.unbox_channel chanr in
          Session.link (out1, in1) (out2, in2);
          unblock out1;
          unblock out2;
          apply_cont cont env Value.unit
       | "%ap_cancel", [chan] ->
          Session.cancel (Value.unbox_channel chan) >>= fun _ ->
          apply_cont cont env Value.unit
       | "%ap_close", [chan] ->
          Session.close (Value.unbox_channel chan);
          apply_cont cont env Value.unit
       (* end of session stuff *)
       | "%route_add_unsafe", [pathv; handler; error_handler] ->
          let path = Value.unbox_string pathv in
          let is_dir_handler = String.length path > 0 && path.[String.length path - 1] = '/' in
          let path = if String.length path == 0 || path.[0] <> '/' then "/" ^ path else path in
          let path =
            match Settings.get (Webserver_types.internal_base_url) with
            | None -> path
            | Some base_url ->
               let base_url = Utility.strip_slashes base_url in
               "/" ^ base_url ^ path
          in
          Webs.add_route is_dir_handler path (Right {Webs.request_handler = (env, handler); Webs.error_handler = (env, error_handler)});
          apply_cont cont env Value.unit
       | "%route_add_static", [uriv; pathv; mime_typesv] ->
          if not (!allow_static_routes) then
            eval_error "Attempt to add a static route after they have been disabled";
          let uri = Value.unbox_string uriv in
          let uri = if String.length uri == 0 || uri.[0] <> '/' then "/" ^ uri else uri in
          let uri =
            match Webs.get_internal_base_url () with
            | None -> uri
            | Some base_uri ->
               let base_uri = Utility.strip_slashes base_uri in
               "/" ^ base_uri ^ uri
          in
          let path = Value.unbox_string pathv in
          let mime_types = List.map (fun v -> let (x, y) = Value.unbox_pair v in (Value.unbox_string x, Value.unbox_string y)) (Value.unbox_list mime_typesv) in
          Webs.add_route true uri (Left (path, mime_types));
          apply_cont cont env Value.unit
       | "%serve_pages", [] ->
          if not (Settings.get (dynamic_static_routes)) then
            allow_static_routes := false;
          begin
            Webs.start env >>= fun () ->
            apply_cont cont env Value.unit
          end
       | "%serve_websockets", [] ->
          Webs.set_accepting_websocket_requests true;
          apply_cont cont env Value.unit
       | _, _ ->
          let object_name = Value.Primitive.object_name desc in
          let location = Value.Primitive.location desc in
          if Location.is_client location
          then client_call (Value.Env.request_data env) object_name cont args
          else let fn = Builtins.find object_name in
               let result = Builtins.apply fn (Value.Env.request_data env) args in
               apply_cont cont env result
       end
    | `Continuation c,      [p] -> apply_cont c env p
    | `Continuation _,       _  ->
       eval_error "Continuation applied to multiple (or zero) arguments"
    | `Resumption r, vs ->
       resume env cont r vs
    | v, _ -> type_error ~action:"apply" "function" v
  and resume env (cont : continuation) (r : resumption) vs =
    Proc.yield (fun () -> K.Eval.resume ~env cont r vs)
  and apply_cont (cont : continuation) env v =
    Proc.yield (fun () -> K.Eval.apply ~env cont v)
  and computation_yielding env cont body =
    Proc.yield (fun () -> computation env cont body)
  and computation env (cont : continuation) (bindings, tailcomp) : result =
    match bindings with
      | [] -> tail_computation env cont tailcomp
      | b::bs ->
         match b with
         | Let ((var, _) as b, (_, tc)) ->
            let locals = Value.Env.localise env var in
            let cont' =
              K.(let frame = Frame.make (Var.scope_of_binder b) var locals (bs, tailcomp) in
                 frame &> cont)
            in
            tail_computation env cont' tc
         (* function definitions are stored in the global fun map *)
         | Fun _ ->
            computation env cont (bs, tailcomp)
         | Rec _ ->
            computation env cont (bs, tailcomp)
         | Alien alien ->
            let binder = Alien.binder alien in
            let var = Var.var_of_binder binder in
            let scope = Var.scope_of_binder binder in
            let object_name = Alien.object_name alien in
            let desc =
              let location = Alien.location alien in
              let user_name = Var.name_of_binder binder in
              Value.Primitive.make ~var ~user_friendly_name:user_name ~object_name ~location ()
            in
            let open ForeignLanguage in
            let env' =
              match Alien.language alien with
              | Builtin when not (Alien.is_function alien) ->
                 Value.Env.bind var (Builtins.find_value object_name, scope) env
              | _ ->
                 if Alien.is_function alien
                 then Value.Env.bind var (`PrimitiveFunction desc, scope) env
                 else raise (runtime_error "alien ground values are unsupported.")
            in
            computation env' cont (bs, tailcomp)
         | Module _ -> raise (internal_error "Not implemented interpretation of modules yet")
  and tail_computation env (cont : continuation) : Ir.tail_computation -> result = function
    | Ir.Return v   ->
        value env v >>= fun v ->
        apply_cont cont env v
    | Apply (f, ps) ->
        value env f >>= fun f ->
        LwtHelpers.sequence (List.map (value env) ps) >>= fun ps ->
        apply cont env (f, ps)
    | Special s     -> special env cont s
    | Case (v, cases, default) ->
      value env v >>= fun v ->
      begin match v with
        | `Variant (label, _) as v ->
          begin
            match StringMap.lookup label cases, default, v with
            | Some ((var,_), c), _, `Variant (_, v)
            | _, Some ((var,_), c), v ->
              computation (Value.Env.bind var (v, Scope.Local) env) cont c
            | None, _, #Value.t -> eval_error "Pattern matching failed on %s" label
            | _ -> assert false (* v not a variant *)
          end
        | v -> type_error ~action:"take case of" "variant" v
      end
    | If (c,t,e)    ->
        value env c >>= fun c ->
        computation env cont
          (match c with
             | `Bool true     -> t
             | `Bool false    -> e
             | _              -> eval_error "Conditional was not a boolean")
  and special env (cont : continuation) : Ir.special -> result =
    let get_lens l = match l with | `Lens l -> l | _ -> raise (internal_error "Expected a lens.") in
    let invoke_session_exception () =
      special env cont (DoOperation (Value.session_exception_operation,
        [], `Not_typed)) in
    function
    | Wrong _                    -> raise Exceptions.Wrong
    | Database v                 ->
        value env v >>= fun v ->
        apply_cont cont env (`Database (db_connect v))
    | Lens (table, t) ->
      let open Lens in
      begin
          let sort = Type.sort t in
          value env table >>= fun table ->
          match table with
            | `Table (((db,_), table, _, _) as tinfo) ->
              let database = Lens_database_conv.lens_db_of_db db in
              let sort = Sort.update_table_name sort ~table in
              let table = Lens_database_conv.lens_table_of_table tinfo in
                 apply_cont cont env (`Lens (Value.Lens { sort; database; table; }))
            | `List records ->
              let records = List.map Lens_value_conv.lens_phrase_value_of_value records in
              apply_cont cont env (`Lens (Value.LensMem { records; sort; }))
            | _ -> raise (internal_error ("Unsupported underlying lens value."))
      end
    | LensSerial { lens; columns; _ } ->
      let open Lens in
      value env lens >>= fun lens ->
      let lens = get_lens lens |> Value.set_serial ~columns in
      apply_cont cont env (`Lens lens)
    | LensDrop {lens; drop; key; default; _} ->
        let open Lens in
        value env lens >|= get_lens >>= fun lens ->
        value env default >|= Lens_value_conv.lens_phrase_value_of_value >>= fun default ->
        let sort =
          Lens.Sort.drop_lens_sort
            (Lens.Value.sort lens)
            ~drop:[drop]
            ~default:[default]
            ~key:(Alias.Set.singleton key)
          |> Lens_errors.unpack_type_drop_lens_result ~die:(eval_error "%s")
        in
        apply_cont cont env (`Lens (Value.LensDrop { lens; drop; key; default; sort }))
    | LensSelect { lens; predicate; _ } ->
        let open Lens in
        value env lens >|= get_lens >>= fun lens ->
        let predicate =
          match predicate with
          | Static predicate -> predicate
          | Dynamic predicate ->
            let p = Lens_ir_conv.lens_sugar_phrase_of_ir predicate env
                    |> Lens_ir_conv.Of_ir_error.unpack_exn ~die:(eval_error "%s") in
            p in
        let sort =
          Lens.Sort.select_lens_sort
            (Lens.Value.sort lens)
            ~predicate
          |> Lens_errors.unpack_sort_select_result ~die:(eval_error "%s")
        in
        apply_cont cont env (`Lens (Value.LensSelect {lens; predicate; sort}))
    | LensJoin { left; right; on; del_left; del_right; _ } ->
        let open Lens in
        value env left >|= get_lens >>= fun lens1 ->
        value env right >|= get_lens >>= fun lens2 ->
        let left, right=
          if Lens.Sort.join_lens_should_swap
               (Lens.Value.sort lens1)
               (Lens.Value.sort lens2) ~on
          then lens2, lens1
          else lens1, lens2
        in
        let on = List.map (fun a -> a, a, a) on in
        let sort, on =
          Lens.Sort.join_lens_sort
            (Lens.Value.sort lens1)
            (Lens.Value.sort lens2) ~on
          |> Lens_errors.unpack_sort_join_result ~die:(eval_error "%s") in
        apply_cont cont env (`Lens (Value.LensJoin {left; right; on; del_left; del_right; sort}))
    | LensCheck (lens, _typ) ->
        value env lens >>= apply_cont cont env
    | LensGet (lens, _rtype) ->
        value env lens >|= get_lens >>= fun lens ->
        (* let callfn = fun fnptr -> fnptr in *)
        let res = Lens.Value.lens_get lens in
        let res = List.map Lens_value_conv.value_of_lens_phrase_value res |> Value.box_list in
          apply_cont cont env res
    | LensPut (lens, data, _rtype) ->
        value env lens >|= get_lens >>= fun lens ->
        value env data >|= Value.unbox_list >>= fun data ->
        let data = List.map Lens_value_conv.lens_phrase_value_of_value data in
        let behaviour =
          if Settings.get Lens.classic_lenses
          then Lens.Eval.Classic
          else Lens.Eval.Incremental in
        Lens.Eval.put ~behaviour lens data |> Lens_errors.unpack_eval_error ~die:(eval_error "%s");
        Value.box_unit () |> apply_cont cont env
    | Table (db, name, keys, (readtype, _, _)) ->
      begin
        (* OPTIMISATION: we could arrange for concrete_type to have
           already been applied here *)
        value env db >>= fun db ->
        value env name >>= fun name ->
        value env keys >>= fun keys ->
        match db, name, keys, (TypeUtils.concrete_type readtype) with
          | `Database (db, params), name, keys, `Record row ->
            let unboxed_keys =
              List.map
                (fun key ->
                  List.map Value.unbox_string (Value.unbox_list key))
                (Value.unbox_list keys)
            in apply_cont cont env (`Table ((db, params), Value.unbox_string name, unboxed_keys, row))
          | _ -> eval_error "Error evaluating table handle"
      end
    | Query (range, policy, e, _t) ->
       begin
         match range with
           | None -> Lwt.return None
           | Some (limit, offset) ->
              value env limit >>= fun limit ->
              value env offset >>= fun offset ->
              Lwt.return (Some (Value.unbox_int limit, Value.unbox_int offset))
       end >>= fun range ->
       let evaluator =
         let open QueryPolicy in
         match policy with
           | Flat -> `Flat
           | Nested -> `Nested
           | Default ->
               if Settings.get Database.shredding then `Nested else `Flat in

       let evaluate_standard () =
         match EvalQuery.compile env (range, e) with
           | None -> computation env cont e
           | Some (db, q, t) ->
               let q = Sql.string_of_query db range q in
               let (fieldMap, _, _), _ =
               Types.unwrap_row(TypeUtils.extract_row t) in
               let fields =
               StringMap.fold
                   (fun name t fields ->
                     match t with
                     | `Present t -> (name, t)::fields
                     | `Absent -> assert false
                     | `Var _ -> assert false)
                   fieldMap
                   []
               in
               apply_cont cont env (Database.execute_select fields q db) in

       let evaluate_nested () =
         if range != None then eval_error "Range is not supported for nested queries";
           match EvalNestedQuery.compile_shredded env e with
           | None -> computation env cont e
           | Some (db, p) ->
              if db#supports_shredding () then
                let get_fields t =
                  match t with
                  | `Record fields ->
                     StringMap.to_list (fun name p -> (name, `Primitive p)) fields
                  | _ -> assert false
                in
                let execute_shredded_raw (q, t) =
                  let q = Sql.string_of_query db range q in
                  Database.execute_select_result (get_fields t) q db, t in
                let raw_results =
                  EvalNestedQuery.Shred.pmap execute_shredded_raw p in
                let mapped_results =
                  EvalNestedQuery.Shred.pmap EvalNestedQuery.Stitch.build_stitch_map raw_results in
                apply_cont cont env
                  (EvalNestedQuery.Stitch.stitch_mapped_query mapped_results)
              else
                let error_msg =
                  Printf.sprintf
                    "The database driver '%s' does not support shredding."
                    (db#driver_name ())
                in
                raise (Errors.runtime_error error_msg) in

       begin
         match evaluator with
           | `Flat -> evaluate_standard ()
           | `Nested -> evaluate_nested ()
       end
    | InsertRows (source, rows) ->
        begin
          value env source >>= fun source ->
          value env rows >>= fun rows ->
          match source, rows with
          | `Table _, `List [] ->  apply_cont cont env Value.unit
          | `Table ((db, _params), table_name, _, _), rows ->
              let (field_names,vss) = Value.row_columns_values db rows in
              Debug.print ("RUNNING INSERT QUERY:\n" ^ (db#make_insert_query(table_name, field_names, vss)));
              let () = ignore (Database.execute_insert (table_name, field_names, vss) db) in
              apply_cont cont env Value.unit
          | _ -> raise (internal_error "insert row into non-database")
        end
    (* FIXME:

       Choose a semantics for InsertReturning.

       Currently it is well-defined if exactly one row is inserted,
       but is not necessarily well-defined otherwise.

       Perhaps the easiest course of action is to restrict it to the
       case of inserting a single row.
     *)
    | InsertReturning (source, rows, returning) ->
        begin
          value env source >>= fun source ->
          value env rows >>= fun rows ->
          value env returning >>= fun returning ->
          match source, rows, returning with
          | `Table _, `List [], _ ->
              raise (internal_error "InsertReturning: undefined for empty list of rows")
          | `Table ((db, _params), table_name, _, _), rows, returning ->
              let (field_names,vss) = Value.row_columns_values db rows in
              let returning = Value.unbox_string returning in
              Debug.print ("RUNNING INSERT ... RETURNING QUERY:\n" ^
                           String.concat "\n"
                             (db#make_insert_returning_query(table_name, field_names, vss, returning)));
              apply_cont cont env (Database.execute_insert_returning (table_name, field_names, vss, returning) db)
          | _ -> raise (internal_error "insert row into non-database")
        end
    | Update ((xb, source), where, body) ->
      begin
        value env source >>= fun source ->
        match source with
          | `Table ((db, _), table, _, (fields, _, _)) ->
              Lwt.return
            (db, table, (StringMap.map (function
                                        | `Present t -> t
                                        | _ -> assert false) fields))
          | _ -> assert false
      end >>= fun (db, table, field_types) ->
      let update_query =
        Query.compile_update db env ((Var.var_of_binder xb, table, field_types), where, body) in
      let () = ignore (Database.execute_command update_query db) in
        apply_cont cont env Value.unit
    | Delete ((xb, source), where) ->
        value env source >>= fun source ->
        begin
        match source with
          | `Table ((db, _), table, _, (fields, _, _)) ->
              Lwt.return
            (db, table, (StringMap.map (function
                                        | `Present t -> t
                                        | _ -> assert false) fields))
          | _ -> assert false
        end >>= fun (db, table, field_types) ->
      let delete_query =
        Query.compile_delete db env ((Var.var_of_binder xb, table, field_types), where) in
      let () = ignore (Database.execute_command delete_query db) in
        apply_cont cont env Value.unit
    | CallCC f ->
       value env f >>= fun f ->
       apply cont env (f, [`Continuation cont])
    (* Handlers *)
    | Handle { ih_comp = m; ih_cases = clauses; ih_return = return; ih_depth = depth } ->
       (* Slight hack *)
       begin
         match depth with
         | Shallow -> Lwt.return (env, `Shallow)
         | Deep params ->
            LwtHelpers.foldr_lwt
              (fun (b, initial_value) (env, vars) ->
                let var = Var.var_of_binder b in
                value env initial_value >>= fun initial_value ->
                Lwt.return (Value.Env.bind var (initial_value, Scope.Local) env, var :: vars))
              params (Lwt.return (env, [])) >>= fun (env, vars) ->
            Lwt.return (env, `Deep vars)
       end >>= fun (env, depth) ->
       let handler = K.Handler.make ~env ~return ~clauses ~depth in
       let cont = K.set_trap_point ~handler cont in
       computation env cont m
    | DoOperation (name, vs, _) ->
       let open Value.Trap in
       begin
         match List.map (value env) vs with
         | [v] -> v
         | vs  ->
             LwtHelpers.sequence vs >>= fun vs ->
             Lwt.return (Value.box vs)
       end >>= fun v ->
       begin
       match K.Eval.trap cont (name, v) with
         | Trap cont_thunk -> cont_thunk ()
         | SessionTrap st_res ->
             handle_session_exception env st_res.frames >>= fun _ ->
             st_res.continuation_thunk ()
         | UnhandledSessionException frames ->
             Debug.print ("unhandled session exception");
             handle_session_exception env frames >>= fun _ ->
             Proc.finish (env, Value.box_unit())
       end
    (* Session stuff *)
    | Select (name, v) ->
      value env v >>= fun chan ->
      Debug.print ("selecting: " ^ name ^ " from: " ^ Value.string_of_value chan);
      let ch = Value.unbox_channel chan in
      let (outp, _inp) = ch in
      Session.send_from_local (Value.box_variant name Value.unit) outp >>= fun _ ->
      OptionUtils.opt_iter Proc.awaken (Session.unblock outp);
      apply_cont cont env chan
    | Choice (v, cases) ->
      begin
        let open Session in
        value env v >>= fun chan ->
        Debug.print("choosing from: " ^ Value.string_of_value chan);
        let unboxed_chan = Value.unbox_channel chan in
        let inp = receive_port unboxed_chan in
        let block () =
          let choice_frame =
             K.Frame.of_expr env (Special (Choice (v, cases)))
          in
             Session.block inp (Proc.get_current_pid ());
             Proc.block (fun () -> apply_cont K.(choice_frame &> cont) env Value.unit)
        in
        match Session.receive unboxed_chan with
          | ReceiveOK v ->
            let label = fst @@ Value.unbox_variant v in
            Debug.print ("chose label: " ^ label);
              begin
                match StringMap.lookup label cases with
                | Some ((var,_), body) ->
                  computation (Value.Env.bind var (chan, Scope.Local) env) cont body
                | None -> eval_error "Choice pattern matching failed"
              end
          | ReceiveBlocked -> block ()
          | ReceivePartnerCancelled ->
              (* If session exceptions enabled, then cancel this endpoint and
               * invoke the session exception. Otherwise, block, as per old semantics. *)
              if (Settings.get Basicsettings.Sessions.exceptions_enabled) then
                begin
                  Session.cancel unboxed_chan >>= fun _ ->
                  invoke_session_exception ()
                end
              else block ()
      end
  and finish env v = Proc.finish (env, v)
    (*****************)

  let reify (r : resumption) = `Resumption r
  let eval : Value.env -> program -> result =
    fun env -> computation env K.empty

  let run_program : Value.env -> Ir.program -> (Value.env * Value.t) =
    fun env program ->
      try (
        Proc.start (fun () -> eval env program)
      ) with
        | NotFound s ->
            raise (internal_error ("NotFound " ^ s ^
              " while interpreting."))
        | Not_found  -> raise (internal_error ("Not_found while interpreting."))
end

module type EVAL = functor (Webs : WEBSERVER) -> sig
    include EVALUATOR
end
module Eval : EVAL = functor (Webs : WEBSERVER) ->
struct
  module rec Eval : EVALUATOR
    with type result = Proc.thread_result Lwt.t = Evaluator(Value.Continuation.Evaluation(Eval))(Webs)
  include Eval
end
