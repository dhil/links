(* Desugars alien block declarations into plain alien declarations.
  alien javascript {
    setTitle : (String) ~> () = "_setTitle";
    alertBox : (String) ~> () = "_alertBox";
  }

  --->

  alien javascript setTitle client : (String) ~> () = "_setTitle";
  alien javascript alertBox client : (String) ~> () = "_alertBox";

  Note that, for now, FFI JavaScript entities are assumed to live on
  the client side.
 *)
open SourceCode.WithPos
open Sugartypes

let desugar_alien : 'a. SourceCode.Position.t -> 'a Alien.t -> binding list
  = fun pos alien ->
  List.map
    (fun entity ->
      let alien =
        let entity' =
          let open CommonTypes.ForeignLanguage in
          let open CommonTypes.Location in
          match Alien.language alien, Alien.Entity.location entity with
          | JavaScript, Unknown -> (* HACK: Currently FFI JavaScript
                                      entities are always assumed to
                                      live on the client. *)
             Alien.Entity.modify ~location:Client entity
          | _, _ -> entity
        in
        Foreign (Alien.single
                   (Alien.language alien)
                   entity')
      in
      SourceCode.WithPos.make ~pos alien)
    (Alien.declarations alien)

let rec desugar_aliens = function
  | [] -> []
  | b :: bs ->
     match node b with
     | AlienBlock alien ->
        let aliens = desugar_alien (pos b) alien in
        aliens :: desugar_aliens bs
     | Foreign alien ->
        let aliens = desugar_alien (pos b) alien in
        aliens :: desugar_aliens bs
     | _ -> [b] :: desugar_aliens bs

let desugar =
  object
    inherit SugarTraversals.map

    method! program (bs, comp) =
      let bs' = List.concat (desugar_aliens bs) in
      (bs', comp)

    method! sentence = function
      | Definitions bs ->
         let bs' = List.concat (desugar_aliens bs) in
         Definitions bs'
      | sentence -> sentence
  end

let program = desugar#program

(* This has the same problem as the above (c.f. issue #604). *)
let sentence = desugar#sentence

