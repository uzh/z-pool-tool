type t = Ptime.t

val date_to_human : Ptime.date -> string
val time_to_human : ?with_seconds:bool -> Ptime.date * Ptime.time -> string
val now : unit -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val sexp_of_t : t -> Sexplib0.Sexp.t
val equal_date : Ptime.date -> Ptime.date -> bool
val sexp_of_span : Ptime.Span.t -> Sexplib0.Sexp.t
val ptime_span_of_yojson : Yojson.Safe.t -> Ptime.Span.t
val yojson_of_ptime_span : Ptime.Span.t -> Yojson.Safe.t
val formatted_timespan : Ptime.Span.t -> string
val print_time_span : Ptime.Span.t -> string
val date_of_yojson : Yojson.Safe.t -> Ptime.date
val yojson_of_date : Ptime.date -> Yojson.Safe.t
val decimal : int -> string
val formatted_date_time : t -> string
val formatted_date : t -> string
val timespan_to_minutes : Ptime.Span.t -> string
val format_start_end_with_duration : t -> Ptime.Span.t -> string
val format_start_end : t -> Ptime.Span.t -> string

module Parsing : sig
  val parse_date_time : string -> (t, string) result
  val time_span : string -> (Ptime.span, string) result
  val date : string -> (Ptime.date, string) result
  val from_calendar : string -> (t, string) result
end
