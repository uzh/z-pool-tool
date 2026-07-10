(** Base time functionality on top of [Ptime], without dependencies on other
    project libraries. Conformist schemas and [Pool_message]-typed parsers
    live in [Pool_model.Time]. *)

include module type of struct
    include Ptime
  end
  with module Span := Ptime.Span

(** The time units of [Duration], doubling as user-facing selector values.
    Form schemas and error handling live in [Pool_model.Base.TimeUnit]. *)
module TimeUnit : sig
  type t =
    | Seconds
    | Minutes
    | Hours
    | Days

  val min : int
  val max : int
  val to_enum : t -> int
  val of_enum : int -> t option
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t

  (** All units, in ascending order. *)
  val all : t list

  (** [read str] parses a unit from its serialised name (e.g. "minutes").
      Raises on unknown names; see [Pool_model.Base.TimeUnit.of_string] for
      the error-typed variant. *)
  val read : string -> t

  val default_unit : t
  val to_human : t -> string

  (** Seconds per unit. *)
  val factor : t -> int
end

(** An amount of a time unit: [(10, Minutes)] is ten minutes. Convert with
    [Span.of_duration]. *)
module Duration : sig
  type t = int * TimeUnit.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val to_seconds : t -> int
end

(** Calendar dates without a time of day ([Ptime.date] triples). Timezone-free:
    converting an instant to a date happens before, e.g. via [to_local_date]. *)
module Date : sig
  type t = Ptime.date

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  (** ISO 8601 ("YYYY-MM-DD"); also the DB and JSON wire format. *)
  val to_string : t -> string

  (** Display rendering ("DD.MM.YYYY"). *)
  val to_human : t -> string

  (** RFC 3339 at midnight UTC, as flatpickr expects. *)
  val to_flatpickr : t -> string

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Span : sig
  include module type of struct
    include Ptime.Span
  end

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val value : t -> Ptime.Span.t
  val of_span : Ptime.Span.t -> t
  val to_int_s_exn : span -> tz_offset_s

  (** Humanized rendering via [Ptime.Span.pp], e.g. "30min" or "2h". *)
  val to_human : t -> string

  val of_duration : Duration.t -> t

  (** Per-unit constructors: [hours 2] is a two-hour span. *)
  val seconds : int -> t

  val minutes : int -> t
  val hours : int -> t
  val days : int -> t

  (** The longer of two spans. *)
  val max : t -> t -> t

  (** Decompose a span into the largest unit that divides it evenly, e.g.
      7200s becomes [(2, Hours)]. [units] restricts the candidates;
      non-positive spans fall back to their total seconds. *)
  val to_duration : ?units:TimeUnit.t list -> t -> Duration.t
end

(** Current instant from the system clock. [Ptime.t] carries no timezone —
    the value is a POSIX instant. Use this instead of calling
    [Ptime_clock.now] directly. *)
val now : unit -> t

(* [equal] (tolerant in tests) and [to_rfc3339] (explicit UTC marker by
   default) intentionally shadow the [Ptime] versions included above. *)

val show : t -> string
val value : t -> Ptime.t
val create : Ptime.t -> t

(** A display timezone as UTC offsets plus a DST predicate over UTC instants.
    Storage and computation stay UTC; only rendering converts. *)
module Timezone : sig
  type t =
    { std_offset_s : int
    ; dst_offset_s : int
    ; is_dst : Ptime.t -> bool
    }

  val europe_zurich : t
  val to_offset_s : t -> Ptime.t -> int
end

val display_timezone : Timezone.t
val to_zurich_tz_offset_s : Ptime.t -> int
val to_local_date : Ptime.t -> Ptime.t

(* Formatting, in the display timezone. Spans are rendered by [Span.to_human],
   calendar dates by [Date.to_human]. *)
val formatted_date : Ptime.t -> string
val formatted_date_time : Ptime.t -> string
val format_start_end : Ptime.t -> Ptime.Span.t -> string
val format_start_end_with_duration : Ptime.t -> Ptime.Span.t -> string

(* (De)serialisers *)
val ptime_to_sexp : Ptime.t -> Sexplib0.Sexp.t
val ptime_of_yojson : Yojson.Safe.t -> Ptime.t
val yojson_of_ptime : Ptime.t -> Yojson.Safe.t
val ptime_span_to_sexp : Ptime.Span.t -> Sexplib0.Sexp.t
val ptime_span_of_yojson : Yojson.Safe.t -> Ptime.Span.t
val yojson_of_ptime_span : Ptime.Span.t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val sexp_of_t : t -> Sexplib0.Sexp.t
