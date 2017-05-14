(*pp deriving *)
(* Values and environments *)
open ProcessTypes

class type otherfield
 = object method show : string end

type db_status = [ `QueryOk | `QueryError of string ]

class virtual dbvalue :
  object
    method virtual error : string
    method virtual fname : int -> string
    method virtual get_all_lst : string list list
    method virtual nfields : int
    method virtual ntuples : int
    method map : 'a. ((int -> string) -> 'a) -> 'a list
    method map_array : 'a. (string array -> 'a) -> 'a list
    method fold_array : 'a. (string array -> 'a -> 'a) -> 'a -> 'a
    method virtual getvalue : int -> int -> string
    method virtual gettuple : int -> string array
    method virtual status : db_status
  end

class virtual database :
  object
    method virtual driver_name : unit -> string
    method virtual escape_string : string -> string
    method virtual quote_field : string -> string
    method virtual exec : string -> dbvalue
    method make_insert_query : (string * string list * string list list) -> string
    method make_insert_returning_query : (string * string list * string list list * string) -> string list
  end

module Eq_database : Deriving_Eq.Eq with type a = database
module Typeable_database : Deriving_Typeable.Typeable with type a = database
module Show_database : Deriving_Show.Show with type a = database

type db_constructor = string -> database * string

val register_driver : string * db_constructor -> unit
val db_connect : string -> string -> database * string
val parse_db_string : string -> string * string
val reconstruct_db_string : string * string -> string

type xmlitem =   Text of string
               | Attr of (string * string)
               | Node of (string * xml)
and xml = xmlitem list
  deriving (Show)

type table = (database * string) * string * string list list * Types.row
  deriving (Show)

type primitive_value = [
| `Bool of bool
| `Char of char
| `Database of (database * string)
| `Table of table
| `Float of float
| `Int of int
| `XML of xmlitem
| `String of string ]

module Show_primitive_value : Deriving_Show.Show with type a = primitive_value

type spawn_location = [
  | `ClientSpawnLoc of client_id
  | `ServerSpawnLoc (* Will need to add in a server address when we go to n-tier *)
]
  deriving (Show)

type dist_pid = [
  | `ClientPid of (client_id * process_id)
  | `ServerPid of process_id (* Again, will need a server address here later *)
]
  deriving (Show)

type access_point = [
  | `ClientAccessPoint of (client_id * apid)
  | `ServerAccessPoint of apid
]
  deriving (Show)

type chan = (channel_id * channel_id)

module type VENV = sig
  type 'v t

  val set_request_data : 'v t -> RequestData.request_data -> 'v t
  val empty : 'v t
  val bind  : Ir.var -> (t * Ir.scope) -> 'v t -> 'v t
  val find : Ir.var -> 'v t -> t
  val mem : Ir.var -> 'v t -> bool
  val lookup : Ir.var -> 'v t -> t option
  val lookupS : Ir.var -> 'v t -> (t * Ir.scope) option
  val shadow : 'v t -> by:'v t -> 'v t
  val fold : (Ir.var -> (t * Ir.scope) -> 'a -> 'a) -> 'v t -> 'a -> 'a
  val globals : 'v t -> 'v t
  val request_data : 'v t -> RequestData.request_data
  (* used only by json.ml, webif.ml ... *)
  val get_parameters : 'v t -> (t*Ir.scope) Utility.intmap
  val extend : 'v t -> (t*Ir.scope) Utility.intmap -> 'v t
  val localise : 'v t -> Ir.var -> 'v t

  type 'cv ct
  val compress   : ('v -> 'cv) -> 'v t -> 'cv ct
  val uncompress : ('cv -> 'v) -> 'cv ct -> 'v t
end

module VEnv : VENV

(* Continuation *)
module type CONTINUATION = sig
  type 'v env = 'v VEnt.t
  type ('v env, 'r) t

  module Frame : sig
    type nonrec 'env t
    val of_expr : 'env -> Ir.tail_computation -> 'env t
    val make : Ir.scope -> Ir.var -> 'env -> Ir.computation -> 'env t
  end

  val empty : ('v VEnv.t, 'r) t
  val (<>)  : ('v VEnv.t, 'r) t -> ('v VEnv.t, 'r) t -> ('v VEnv.t, 'r) t
  val (&>)  : ('v VEnt.t) Frame.t -> ('v VEnv.t, 'r) t -> ('v VEnv.t, 'r) t

  val apply : ~eval:('v VEnv.t -> ('v VEnt.t, 'r) -> Ir.computation -> 'r) ->
              ~env:'v VEnv.t -> ('v VEnv.t, 'r) t -> 'v -> 'r

  val to_string : ('v VEnv.t, 'r) t -> string

  type ('cv, r) ct
  val compress   : ('v -> 'cv) -> ('v VEnv.t, 'r) t -> ('cv, 'r) ct
  val uncompress : ('cv -> 'v) -> ('cv, 'r) ct -> ('v  VEnv.t, 'r) t
end

module Continuation : CONTINUATION

type t = [
| primitive_value
| `List of t list
| `Record of (string * t) list
| `Variant of string * t
| `FunctionPtr of (Ir.var * t option)
| `PrimitiveFunction of string * Var.var option
| `ClientFunction of string
| `Continuation of continuation
| `Pid of dist_pid
| `AccessPointID of access_point
| `SessionChannel of chan
| `Socket of in_channel * out_channel
| `SpawnLocation of spawn_location
]
and continuation = (env, 'r) Continuation.t
and env = t VEnv.t
    deriving (Show)

type delegated_chan = (chan * (t list))

val project : string -> [> `Record of (string * 'b) list ] -> 'b
val untuple : t -> t list

val box_bool : 'a -> [> `Bool of 'a ]
val unbox_bool : t -> bool
val box_int : 'a -> [> `Int of 'a ]
val unbox_int : t -> int
val box_float : 'a -> [> `Float of 'a ]
val unbox_float : t -> float
val box_char : 'a -> [> `Char of 'a ]
val unbox_char : t -> char
val box_xml : 'a -> [> `XML of 'a ]
val unbox_xml : t -> xmlitem
val box_string : string -> t
val unbox_string : t -> string
val box_list : t list -> t
val unbox_list : t -> t list
val box_record : (string * t) list -> t
val unbox_record : t -> (string * t) list
val box_unit : unit -> t
val unbox_unit : t -> unit
val box_pair : t -> t -> t
val unbox_pair : t -> (t * t)
val box_pid : dist_pid -> t
val unbox_pid : t -> dist_pid
val box_socket : in_channel * out_channel -> t
val unbox_socket : t -> in_channel * out_channel
val box_spawn_loc : spawn_location -> t
val unbox_spawn_loc : t -> spawn_location
val box_channel : chan -> t
val unbox_channel : t -> chan
val box_access_point : access_point -> t
val unbox_access_point : t -> access_point

val intmap_of_record : t -> t Utility.intmap option

val string_as_charlist : string -> t
val charlist_as_string : t -> string
val string_of_value : t -> string
val string_of_xml : ?close_tags:bool -> xml -> string
val string_of_primitive : primitive_value -> string
val string_of_tuple : (string * t) list -> string
val string_of_cont : continuation -> string

val marshal_value : t -> string
val marshal_continuation : continuation -> string

val unmarshal_continuation : env -> string -> continuation
val unmarshal_value : env -> string -> t

(* Given a value, retreives a list of channels that are contained inside *)
val get_contained_channels : t -> chan list

val value_of_xmlitem : xmlitem -> t

val split_html : xml -> xml * xml
