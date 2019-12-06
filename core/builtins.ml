(* Backend representation of builtins/primitives. *)
open Utility
open Proc

let runtime_error msg = Errors.runtime_error msg
let internal_error message =
  Errors.internal_error ~filename:"builtins.ml" ~message

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

(* Temporary hack. *)
let pure_builtins =
    [| "+"; "-"; "*"; "/"; "^"; "mod"; "negate"
     ; "+."; "-."; "*."; "/."; "^."; "negatef"
     ; "floor"; "ceiling"; "cos"; "sin"; "tan"; "log"; "log10"; "sqrt"
     ; "=="; "<>"; "<"; ">"; "<="; ">="
     ; "intEq"; "stringEq"; "floatEq"; "floatNeq"; "objectEq"
     ; "intToString"; "intToFloat"; "intoToXml"
     ; "floatToInt"; "floatToString"; "floatToXml"
     ; "stringToInt"; "stringToFloat"; "stringToXml"
     ; "list_nil"; "list_cons"; "++"; "length"; "take"; "drop"; "max"; "min"
     ; "ord"; "chr"
     ; "tilde"; "ltilde"; "stilde"
     ; "strlen"; "strContains"; "implode"; "explode" |]

let is_pure_primitive name = Array.mem name pure_builtins
