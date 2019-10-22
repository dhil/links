(* Compilation environment. *)
open Utility

module PMap = Map.Make(struct type t = Pident.t [@@deriving show] let compare = Pident.compare end)
module PSet = Set.Make(struct type t = Pident.t [@@deriving show] let compare = Pident.compare end)

type var = int
         [@@deriving show]

module Comp_unit = struct
  type kind = Physical of { filename: string }
            | Interactive of { name: string }
  type t =
    { kind: kind;
      mutable next: int;
      ident: Pident.t;
      dependencies: PSet.t }

  let name_of_filename filename =
    String.capitalize_ascii
      Filename.(remove_extension (basename filename))

  let name { kind; _ } =
    match kind with
    | Physical { filename } -> name_of_filename filename
    | Interactive { name } -> name

  let identifier c = c.ident

  let make kind name =
    { kind; next = 1;
      dependencies = PSet.empty;
      ident = Pident.of_name name }

  let of_filename filename =
    make (Physical { filename }) (name_of_filename filename)

  let interactive name =
    make (Interactive { name }) name

  let depend : t -> t -> t
    = fun dependee dependant ->
    let dependencies' =
      PSet.add (identifier dependee) dependant.dependencies
    in
    { dependant with dependencies = dependencies' }

  let depends dependee dependant =
    PSet.mem (identifier dependee) dependant.dependencies

  module Gensym = struct
    let next c =
      let n = c.next in
      c.next <- n + 1; n
  end

  module Ident = struct
    type comp_unit = t
    module Binder = struct
      module Scope = struct
        type t = Local | Global
                 [@@deriving show]

        let is_global = function
          | Global -> true
          | _ -> false

        let is_local x = not (is_global x)
      end

      type t =
        { datatype: Types.datatype;
          scope: Scope.t;
          name: string;
          ident: var;
          host: Pident.t }
          [@@deriving show]

      let fresh : ?datatype:Types.datatype -> ?scope:Scope.t -> comp_unit -> string -> t
        = fun ?(datatype=`Not_typed) ?(scope=Scope.Local) c name ->
        let ident = Gensym.next c in
        { datatype; scope; name; ident; host = identifier c }

      let equal : t -> t -> bool
        = fun x y ->
        x.ident = y.ident && Pident.equal x.host y.host

      let compare x y =
        let result = Pident.compare x.host y.host in
        if result = 0
        then Stdlib.compare x.ident y.ident
        else result
    end

    (* Compilation unit names, interface names. *)
    module Persistent = Pident

    (* Compilation unit local names. *)
    module Local = struct
      type t = var list (* relative path. *)
               [@@deriving show]

      let make : Binder.t list -> t
        = fun bs ->
        List.map (fun b -> b.Binder.ident) bs

      let rec equal xs ys =
        match xs, ys with
        | [], [] -> true
        | x :: xs', y :: ys' ->
           x = y && equal xs' ys'
        | _, _ -> false

      let rec compare xs ys =
        match xs, ys with
        | [], [] -> 0
        | x :: xs', y :: ys' ->
           let result = Stdlib.compare x y in
           if result = 0
           then compare xs' ys'
           else result
        | _x :: _, [] -> 1
        | [], _y :: _ -> (-1)
    end

    (* Compilation unit remote names. *)
    module Remote = struct
      (* TODO(dhil): At the moment this data structure models a
         reference to a concrete member in some compilation
         unit. Thinking further ahead, about separate compilation, a
         reference may be to an interface member. *)
      type t = (* absolute path. *)
        { origin: Persistent.t;
          path: Persistent.t list }
        [@@deriving show]

      let make : comp_unit -> Persistent.t list -> t
        = fun origin path -> { origin = identifier origin; path }

      let equal x y =
        if Persistent.equal x.origin y.origin
        then let rec equal xs ys =
               match xs, ys with
               | [], [] -> true
               | x :: xs', y :: ys' ->
                  Persistent.equal x y && equal xs' ys'
               | _, _ -> false
             in
             equal x.path y.path
        else false

      let compare x y =
        let result = Persistent.compare x.origin y.origin in
        if result = 0
        then let rec compare xs ys =
               match xs, ys with
               | [], [] -> 0
               | x :: xs', y :: ys' ->
                  let result = Persistent.compare x y in
                  if result = 0
                  then compare xs' ys'
                  else result
               | _x :: _, [] -> 1
               | [], _y :: _ -> (-1)
             in
             compare x.path y.path
        else result
    end
  end
end

module Comp_env = struct
  type t = unit
         [@@deriving show]
end
