open CCFun.Infix
include Ptime

(* Formatting *)
let decimal n = if n < 10 then Format.asprintf "0%d" n else Format.asprintf "%d" n

let date_to_human date =
  let year, month, day = date in
  Format.asprintf "%s.%s.%d" (decimal day) (decimal month) year
;;

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

(* Serialise with an explicit UTC marker ("Z"). [Ptime.to_rfc3339]'s default
   is "-00:00", which RFC 3339 defines as "offset unknown" — i.e. a timestamp
   without timezone information. *)
let to_rfc3339 time = Ptime.to_rfc3339 ~tz_offset_s:0 time

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

let equal_date ((y1, m1, d1) : Ptime.date) ((y2, m2, d2) : Ptime.date) =
  CCInt.(equal y1 y2 && equal m1 m2 && equal d1 d2)
;;

let formatted_date ptime = ptime |> to_local_date |> Ptime.to_date |> date_to_human

let formatted_time ?with_seconds ptime =
  ptime |> to_local_date |> Ptime.to_date_time |> time_to_human ?with_seconds
;;

let formatted_date_time (date : Ptime.t) =
  Format.asprintf "%s %s" (formatted_date date) (formatted_time date)
;;

let formatted_timespan timespan =
  Ptime.Span.pp Format.str_formatter timespan;
  Format.flush_str_formatter ()
;;

let timespan_to_minutes timespan =
  timespan
  |> Ptime.Span.to_int_s
  |> CCOption.map_or ~default:"" (fun timespan -> timespan / 60 |> CCInt.to_string)
;;

let format_start_end start duration =
  let end_time =
    Ptime.add_span start duration |> CCOption.get_exn_or "end time not in range"
  in
  let format_end_date =
    if equal_date (Ptime.to_date start) (Ptime.to_date end_time)
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
let ptime_to_sexp p =
  let formatted = p |> formatted_date_time in
  Sexplib0.Sexp.Atom formatted
;;

let ptime_span_to_sexp p =
  let formatted = p |> formatted_timespan in
  Sexplib0.Sexp.Atom formatted
;;

let ptime_span_of_yojson m =
  m
  |> Yojson.Safe.to_string
  |> CCInt.of_string
  |> CCOption.map Ptime.Span.of_int_s
  |> CCOption.get_exn_or "Invalid timespan provided"
;;

module Rfc3339 = struct
  let char = "\""
  let wrap s = Format.asprintf "%s%s%s" char s char
  let unwrap s = CCString.(s |> replace ~sub:char ~by:"")
end

let yojson_of_ptime_span m =
  m
  |> Ptime.Span.to_int_s
  |> CCOption.map (fun i -> i |> CCInt.to_string |> Yojson.Safe.from_string)
  |> CCOption.get_exn_or "Invalid timespan provided"
;;

let ptime_of_yojson m =
  m
  |> Yojson.Safe.to_string
  |> Rfc3339.unwrap
  |> Ptime.of_rfc3339
  |> CCResult.map (fun (m, _, _) -> m)
  |> CCResult.get_exn
;;

let yojson_of_ptime m = m |> Ptime.to_rfc3339 |> Rfc3339.wrap |> Yojson.Safe.from_string

let print_time_span span =
  Ptime.Span.to_int_s span
  |> CCOption.map CCInt.to_string
  (* If span is bigger than int max (signed), print an info string instead *)
  (* This is only the case if span > 136 years (64bit system) *)
  |> CCOption.get_or ~default:"Session duration too long!"
;;

let yojson_of_ptime_date (y, m, d) : Yojson.Safe.t =
  `String (Format.asprintf "%d-%02d-%02d" y m d)
;;

let ptime_date_of_yojson json : Ptime.date =
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

let t_of_yojson = ptime_of_yojson
let yojson_of_t = yojson_of_ptime
let sexp_of_t = ptime_to_sexp
