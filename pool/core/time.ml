open CCFun.Infix
include Ptime

let ppx_printer m fmt _ = Format.pp_print_string fmt m

(* The time units of [Duration], doubling as user-facing selector values.
   Form schemas and error handling live in [Pool_model.Base.TimeUnit]. *)
module TimeUnit = struct
  module Enum = struct
    type t =
      | Seconds [@name "seconds"] [@printer ppx_printer "seconds"]
      | Minutes [@name "minutes"] [@printer ppx_printer "minutes"]
      | Hours [@name "hours"] [@printer ppx_printer "hours"]
      | Days [@name "days"] [@printer ppx_printer "days"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Enum
  include Selector.Make (Enum)

  let default_unit = Minutes
  let to_human = show %> CCString.capitalize_ascii

  let factor = function
    | Seconds -> 1
    | Minutes -> 60
    | Hours -> 60 * 60
    | Days -> 60 * 60 * 24
  ;;
end

(* Durations as an amount of a time unit: [(10, Minutes)] is ten minutes.
   Convert with [Span.of_duration]. *)
module Duration = struct
  type t = int * TimeUnit.t [@@deriving show, eq]

  let to_seconds (amount, unit) = amount * TimeUnit.factor unit
end

(* Formatting *)
let decimal n = if n < 10 then Format.asprintf "0%d" n else Format.asprintf "%d" n

let time_to_human ?(with_seconds = false) time =
  let _, ((h, m, s), _) = time in
  let seconds = if with_seconds then Format.asprintf ":%s" (decimal s) else "" in
  Format.asprintf "%s:%s%s" (decimal h) (decimal m) seconds
;;

(* Public functions *)

(* Current instant from the system clock. [Ptime.t] carries no timezone — the
   value is a POSIX instant. Timezone information is attached at the
   boundaries: [to_rfc3339] below stamps the explicit UTC offset ("Z") on
   every serialised value, and the [formatted_*] functions convert to the
   display timezone. Use this instead of calling [Ptime_clock.now] directly. *)
let now = Ptime_clock.now

(* Serialise with an explicit UTC marker ("Z") by default. [Ptime.to_rfc3339]'s
   default is "-00:00", which RFC 3339 defines as "offset unknown" — i.e. a
   timestamp without timezone information. *)
let to_rfc3339 ?space ?frac_s ?(tz_offset_s = 0) time =
  Ptime.to_rfc3339 ?space ?frac_s ~tz_offset_s time
;;

(* Calendar dates without a time of day ([Ptime.date] triples). Timezone-free:
   converting an instant to a date happens before, e.g. via [to_local_date]. *)
module Date = struct
  type t = Ptime.date

  let equal ((y1, m1, d1) : t) ((y2, m2, d2) : t) =
    CCInt.(equal y1 y2 && equal m1 m2 && equal d1 d2)
  ;;

  (* ISO 8601; also the DB and JSON wire format. *)
  let to_string ((y, m, d) : t) =
    Format.asprintf "%s-%s-%s" (decimal y) (decimal m) (decimal d)
  ;;

  let pp formatter t = CCString.pp formatter (to_string t)

  let to_human ((year, month, day) : t) =
    Format.asprintf "%s.%s.%d" (decimal day) (decimal month) year
  ;;

  let to_flatpickr =
    Ptime.of_date %> CCOption.get_exn_or "Invalid date provided" %> to_rfc3339
  ;;

  let yojson_of_t t : Yojson.Safe.t = `String (to_string t)

  let t_of_yojson json : t =
    let error = "Invalid date format" in
    let str =
      match json with
      | `String s -> s
      | _ -> failwith error
    in
    CCString.split ~by:"-" str
    |> function
    | [ y; m; d ] ->
      let to_int =
        CCString.replace ~which:`Left ~sub:"0" ~by:""
        %> CCInt.of_string
        %> CCOption.get_exn_or error
      in
      to_int y, to_int m, to_int d
    | _ -> failwith error
  ;;
end

(* Tolerant equality in tests: covers second-precision truncation from
   RFC 3339 serialisation and MariaDB DATETIME storage (max diff ≈ 0.999 s). *)
let equal a b =
  if not (Configuration.is_test ())
  then Ptime.equal a b
  else (
    let tolerance_s = 1.0 in
    let diff = Ptime.diff a b |> Ptime.Span.to_float_s |> Float.abs in
    diff <= tolerance_s)
;;

let show = Format.asprintf "%a" pp
let value m = m
let create m = m

(* A display timezone as UTC offsets plus a DST predicate over UTC instants.
   Storage and computation stay UTC; only rendering converts. Derived from the
   EU rule directly so the result does not depend on the host's TZ setting. *)
module Timezone = struct
  type t =
    { std_offset_s : int
    ; dst_offset_s : int
    ; is_dst : Ptime.t -> bool
    }

  let europe_zurich =
    let hour = 60 * 60 in
    (* March and October both have 31 days *)
    let last_sunday year month =
      let weekday_num = function
        | `Sun -> 0
        | `Mon -> 1
        | `Tue -> 2
        | `Wed -> 3
        | `Thu -> 4
        | `Fri -> 5
        | `Sat -> 6
      in
      let last_day =
        (year, month, 31) |> Ptime.of_date |> CCOption.get_exn_or "Invalid date"
      in
      31 - weekday_num (Ptime.weekday last_day)
    in
    (* EU rule: CEST from last Sunday of March, 01:00 UTC, until last Sunday of
       October, 01:00 UTC; CET otherwise. *)
    let transition year month =
      Ptime.of_date_time ((year, month, last_sunday year month), ((1, 0, 0), 0))
      |> CCOption.get_exn_or "Invalid DST transition"
    in
    let is_dst time =
      let year, _, _ = Ptime.to_date time in
      let dst_start = transition year 3 in
      let dst_end = transition year 10 in
      (not (Ptime.is_earlier time ~than:dst_start)) && Ptime.is_earlier time ~than:dst_end
    in
    { std_offset_s = hour; dst_offset_s = 2 * hour; is_dst }
  ;;

  let to_offset_s { std_offset_s; dst_offset_s; is_dst } time =
    if is_dst time then dst_offset_s else std_offset_s
  ;;
end

let display_timezone = Timezone.europe_zurich
let to_zurich_tz_offset_s = Timezone.to_offset_s display_timezone

let to_local_date date =
  let open Ptime in
  date
  |> to_zurich_tz_offset_s
  |> Span.of_int_s
  |> add_span date
  |> CCOption.get_exn_or "Invalid ptime provided"
;;

let formatted_date = to_local_date %> Ptime.to_date %> Date.to_human

let formatted_time ?with_seconds =
  to_local_date %> Ptime.to_date_time %> time_to_human ?with_seconds
;;

let formatted_date_time (date : Ptime.t) =
  Format.asprintf "%s %s" (formatted_date date) (formatted_time date)
;;

let formatted_timespan timespan = Format.asprintf "%a" Ptime.Span.pp timespan

let format_start_end start duration =
  let end_time =
    Ptime.add_span start duration |> CCOption.get_exn_or "end time not in range"
  in
  let format_end_date =
    if Date.equal (Ptime.to_date start) (Ptime.to_date end_time)
    then formatted_time ~with_seconds:false
    else formatted_date_time
  in
  Format.asprintf "%s - %s" (formatted_date_time start) (format_end_date end_time)
;;

let format_start_end_with_duration start duration =
  Format.asprintf
    "%s (%s)"
    (format_start_end start duration)
    (formatted_timespan duration)
;;

(* Utilities *)
let ptime_to_sexp p = Sexplib0.Sexp.Atom (p |> formatted_date_time)
let ptime_span_to_sexp p = Sexplib0.Sexp.Atom (p |> formatted_timespan)

let ptime_span_of_yojson =
  Yojson.Safe.to_string
  %> CCInt.of_string
  %> CCOption.map Ptime.Span.of_int_s
  %> CCOption.get_exn_or "Invalid timespan provided"
;;

module Rfc3339 = struct
  let char = "\""
  let wrap s = Format.asprintf "%s%s%s" char s char
  let unwrap s = CCString.(s |> replace ~sub:char ~by:"")
end

let yojson_of_ptime_span =
  Ptime.Span.to_int_s
  %> CCOption.map (CCInt.to_string %> Yojson.Safe.from_string)
  %> CCOption.get_exn_or "Invalid timespan provided"
;;

let ptime_of_yojson =
  Yojson.Safe.to_string
  %> Rfc3339.unwrap
  %> Ptime.of_rfc3339
  %> CCResult.map (fun (m, _, _) -> m)
  %> CCResult.get_exn
;;

let yojson_of_ptime = to_rfc3339 %> Rfc3339.wrap %> Yojson.Safe.from_string

module Span = struct
  include Ptime.Span

  let sexp_of_t = ptime_span_to_sexp
  let t_of_yojson = ptime_span_of_yojson
  let yojson_of_t = yojson_of_ptime_span
  let value m = m
  let of_span m = m
  let to_human = formatted_timespan
  let of_duration duration = of_int_s (Duration.to_seconds duration)
  let to_int_s_exn span = to_int_s span |> CCOption.get_exn_or "Invalid timespan provided"

  (* Per-unit constructors, e.g. [hours 2]. *)
  let seconds n = of_duration (n, TimeUnit.Seconds)
  let minutes n = of_duration (n, TimeUnit.Minutes)
  let hours n = of_duration (n, TimeUnit.Hours)
  let days n = of_duration (n, TimeUnit.Days)
  let max a b = if compare a b >= 0 then a else b

  (* Decompose a span into the largest unit that divides it evenly, e.g.
     7200s becomes [(2, Hours)]. [units] restricts the candidates;
     non-positive spans fall back to their total seconds. *)
  let to_duration ?(units = TimeUnit.all) span : Duration.t =
    let seconds = to_int_s span |> CCOption.get_or ~default:0 in
    let descending =
      CCList.sort (fun a b -> CCInt.compare (TimeUnit.factor b) (TimeUnit.factor a))
    in
    let rec resolve = function
      | [] -> seconds, TimeUnit.Seconds
      | unit :: smaller ->
        let factor = TimeUnit.factor unit in
        if seconds mod factor = 0 then seconds / factor, unit else resolve smaller
    in
    if seconds > 0 then resolve (descending units) else seconds, TimeUnit.Seconds
  ;;
end

let t_of_yojson = ptime_of_yojson
let yojson_of_t = yojson_of_ptime
let sexp_of_t = ptime_to_sexp
