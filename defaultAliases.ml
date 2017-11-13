(* Alias environment *)
module AliasEnv = Env.String

let alias_env : Types.tycon_environment =
  List.fold_left
    AliasEnv.bind
    AliasEnv.empty
    [
      (* "String"  , `Alias ([], `Application (Types.list, [`Type (`Primitive `Char)])); *)
      "Xml"     , `Alias ([], `Application (Types.list, [`Type (`Primitive `XmlItem)]));
      "Event"   , `Abstract Types.event;
      "List"    , `Abstract Types.list;
      "Process" , `Abstract Types.process;
      "DomNode" , `Abstract Types.dom_node;
      "AP"      , `Abstract Types.access_point;
      "EndBang" , `Alias ([], Types.make_endbang_type);
      "Socket"  , `Abstract Types.socket;
      "Location", `Abstract Types.spawn_location;
      "Ref"     , `Abstract Types.reference;
      "Perf"    , `Abstract Types.performance; (* JavaScript performance clock *)
    ]
