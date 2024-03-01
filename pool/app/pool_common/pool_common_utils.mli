val src : Logs.src
val info : Logs.level
val warning : Logs.level
val error : Logs.level
val debug : Logs.level

module Time : sig
  val decimal : int -> string
  val date_to_human : int * int * int -> string

  val time_to_human
    :  ?with_seconds:bool
    -> 'a * ((int * int * int) * 'b)
    -> string

  val to_zurich_tz_offset_s : Ptime.t -> int
  val to_local_date : Ptime.t -> Ptime.t
  val equal_date : Ptime.date -> Ptime.date -> bool
  val formatted_date : Ptime.t -> string
  val formatted_time : ?with_seconds:bool -> Ptime.t -> string
  val formatted_date_time : Ptime.t -> string
  val formatted_timespan : Ptime.span -> string
  val timespan_to_minutes : Ptime.span -> string
  val format_start_end : Ptime.t -> Ptime.span -> string
  val format_start_end_with_duration : Ptime.t -> Ptime.span -> string
  val ptime_to_sexp : Ptime.t -> Sexplib0.Sexp.t
  val ptime_span_to_sexp : Ptime.span -> Sexplib0.Sexp.t
  val ptime_span_of_yojson : Yojson.Safe.t -> Ptime.span
  val yojson_of_ptime_span : Ptime.span -> Yojson.Safe.t
  val ptime_of_yojson : Yojson.Safe.t -> Ptime.t
  val yojson_of_ptime : Ptime.t -> Yojson.Safe.t
  val print_time_span : Ptime.span -> string
  val parse_time : string -> (Ptime.t, Pool_message.Error.t) result
  val parse_time_span : string -> (Ptime.span, Pool_message.Error.t) result
  val parse_date : string -> (Ptime.date, Pool_message.Error.t) result

  val parse_date_from_calendar
    :  string
    -> (Ptime.t, Pool_message.Error.t) result
end

module PoolConformist : sig
  include Conformist__Core.CONFORMIST with type error_msg = Pool_message.Error.t

  val pp_schema
    :  Format.formatter
    -> ('a, 'b, 'c) t * (string * string list) list
    -> unit

  val decode_and_validate
    :  ?tags:Logs.Tag.set
    -> ('a, 'b, 'c) t
    -> input
    -> ('c, error list) result
end

val handle_ppx_yojson_err
  :  exn * Yojson.Safe.t
  -> ('a, Pool_message.Error.t) result

val handle_json_parse_err : string -> ('a, Pool_message.Error.t) result

val with_log_info
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Info.t
  -> Pool_message.Info.t

val with_log_success
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Success.t
  -> Pool_message.Success.t

val with_log_warning
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Warning.t
  -> Pool_message.Warning.t

val with_log_error
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Error.t
  -> Pool_message.Error.t

val with_log_result_error
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ('a -> Pool_message.Error.t)
  -> ('b, 'a) result
  -> ('b, 'a) result

val decoder
  :  ?tags:Logs.Tag.set
  -> ('a -> ('b, Pool_message.Error.t) result)
  -> Pool_message.Field.t
  -> 'a list
  -> ('b, Pool_message.Error.t) result

val schema_decoder
  :  ?tags:Logs.Tag.set
  -> ?default:'a
  -> (string -> ('a, PoolConformist.error_msg) result)
  -> ('a -> string)
  -> Pool_message.Field.t
  -> ('b, 'a) PoolConformist.Field.t

val schema_list_decoder
  :  'a PoolConformist.decoder
  -> 'a PoolConformist.encoder
  -> Pool_message.Field.t
  -> ('b, 'a) PoolConformist.Field.t
