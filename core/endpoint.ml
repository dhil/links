(* Dynamic linking of web artefacts. *)
open Utility

module Resource = struct
  type t =
    { local_path: string;
      basename: string }

  let make file = (* TODO check whether the resource exists? *)
    { local_path = Filename.dirname file;
      basename = Filename.basename file }

  let basename { basename; _ } = basename

  let to_filename { local_path; basename } =
    Filename.concat local_path basename
end

module WebPath = struct
  type t = string

  let left_trim c path =
    let rec strip c path len ptr =
      if ptr < len
      then if Char.equal c path.[ptr]
           then strip c path len (ptr+1)
           else ptr
      else ptr
    in
    let len = String.length path in
    let start = strip c path len 0 in
    String.sub path start (len - start)

  let right_trim c path =
    let rec strip c path ptr =
      if ptr >= 0
      then if Char.equal c path.[ptr]
           then strip c path (ptr - 1)
           else ptr
      else ptr
    in
    let finish = strip c path (String.length path - 1) in
    if finish >= 0
    then String.sub path 0 (finish + 1)
    else ""

  let trim c path = right_trim c (left_trim c path)

  let normalise s = trim '/' s

  let make s = normalise s (* TODO validate path/url syntax. *)

  let to_string s =
    if String.equal s ""
    then "/"
    else s

  let with_resource : t -> Resource.t -> t
    = fun path resource -> make (Printf.sprintf "%s/%s" (to_string path) (Resource.basename resource))
end


type t =
  { path: WebPath.t;
    resource: Resource.t }

let bind : WebPath.t -> Resource.t -> t
  = fun path resource -> { path; resource }

let to_path : t -> WebPath.t
  = fun { path; resource } ->
  WebPath.with_resource path resource

let to_string : t -> string
  = fun { path; resource } ->
  Printf.sprintf "%s -> %s" (WebPath.to_string path) (Resource.to_filename resource)

let index : int ref = ref (-1)

let endpoints : (WebPath.t, (string * int)) Hashtbl.t =
  Hashtbl.create 7

let get_all () =
  let endpoints' = Array.make (!index + 1) None in
  Hashtbl.iter
    (fun path (resource, index) ->
      Printf.printf "path: %s -> %s\n%!" (WebPath.to_string path) resource;
      let endpoint = bind path (Resource.make resource) in
      Array.set endpoints' index (Some endpoint))
    endpoints;
  Array.(to_list (map (function Some e -> e | None -> assert false) endpoints'))

let endpoint : WebPath.t ref = ref (WebPath.make "/")

let _ =
  (* CLI settings. *)
  let _ = Settings.(option ~default:(Some "/") "endpoint"
                    |> synopsis "Defines a web endpoint"
                    |> hint "<path>"
                    |> privilege `System
                    |> to_string from_string_option
                    |> convert Utility.some
                    |> action (function None -> assert false | Some s -> endpoint := WebPath.make s)
                    |> hidden
                    |> CLI.(add (long "endpoint")))
  in
  Settings.(option "internal_jslib"
            |> synopsis "Serves a given JavaScript library on the latest defined endpoint"
            |> hint "<file>"
            |> privilege `System
            |> to_string from_string_option
            |> convert Utility.some
            |> hidden
            |> action (function
                   | None -> assert false
                   | Some file ->
                      incr index;
                      Hashtbl.add endpoints !endpoint (file, !index) )
            |> CLI.(add (long "jslib"))
            |> sync)

let endpoints
  = let string_of_endpoints _ =
      let endpoints = get_all () in
      String.concat ", " (List.map to_string endpoints)
    in
    Settings.(option ~readonly:true ~default:(Some "") "endpoints"
              |> synopsis "List of bound endpoints"
              |> to_string string_of_endpoints
              |> sync)


