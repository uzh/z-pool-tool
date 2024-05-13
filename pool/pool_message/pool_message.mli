module Field : module type of Field
module Control : module type of Pool_message_control
module Error : module type of Pool_message_error
module Success : module type of Pool_message_success

module Warning : sig
  type t = Warning of string

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val warning : string -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Info : sig
  type t = Info of string

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val info : string -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

type t =
  | Message of string
  | PageNotFoundMessage

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val message : string -> t
val pagenotfoundmessage : t
val sexp_of_t : t -> Sexplib0.Sexp.t

module Collection : sig
  type t =
    { error : Error.t list
    ; warning : Warning.t list
    ; success : Success.t list
    ; info : Info.t list
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val empty : t
  val set_success : Success.t list -> t -> t
  val set_warning : Warning.t list -> t -> t
  val set_error : Error.t list -> t -> t
  val set_info : Info.t list -> t -> t
  val of_string : string -> t option
  val to_string : t -> string
end

val field_message : string -> string -> string -> string

val handle_sihl_login_error
  :  [< `Does_not_exist | `Incorrect_password ]
  -> Error.t

val handle_ppx_yojson_err : exn * Yojson.Safe.t -> ('a, Error.t) result
val handle_json_parse_err : string -> ('a, Error.t) result
val to_conformist_error : (string * 'a * Error.t) list -> Error.t
val add_field_query_params : string -> (Field.t * string) list -> string
