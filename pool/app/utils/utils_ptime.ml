(* Formatting *)
let decimal n =
  if n < 10 then Format.asprintf "0%d" n else Format.asprintf "%d" n
;;

let date_to_human date =
  let year, month, day = date in
  Format.asprintf "%s.%s.%d" (decimal day) (decimal month) year
;;

let time_to_human time =
  let _, ((h, m, _), _) = time in
  Format.asprintf "%s:%s" (decimal h) (decimal m)
;;

(* Public functions *)
let to_local_date date =
  let open Ptime in
  date
  |> to_float_s
  |> Unix.localtime
  |> fun { Unix.tm_isdst; _ } ->
  (if tm_isdst then 2 else 1)
  |> fun rate ->
  rate * 60 * 60
  |> Span.of_int_s
  |> add_span date
  |> CCOption.get_exn_or "Invalid ptime provided"
;;

let formatted_date_time (date : Ptime.t) =
  let date = date |> to_local_date in
  Format.asprintf
    "%s %s"
    (date_to_human (Ptime.to_date date))
    (time_to_human (Ptime.to_date_time date))
;;

let formatted_date ptime =
  ptime |> to_local_date |> Ptime.to_date |> date_to_human
;;

let formatted_time ptime =
  ptime |> to_local_date |> Ptime.to_date_time |> time_to_human
;;

let formatted_timespan timespan =
  Ptime.Span.pp Format.str_formatter timespan;
  Format.flush_str_formatter ()
;;

let timespan_to_hours timespan =
  timespan
  |> Ptime.Span.to_int_s
  |> CCOption.map_or ~default:"" (fun timespan ->
       (timespan |> CCFloat.of_int) /. 3600. |> Format.asprintf "%.2f")
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

let rfc3339_quote = "\""

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

let yojson_of_ptime m =
  m |> Ptime.to_rfc3339 |> Rfc3339.wrap |> Yojson.Safe.from_string
;;

let print_time_span span =
  Ptime.Span.to_int_s span
  |> CCOption.map CCInt.to_string
  (* If span is bigger than int max (signed), print an info string instead *)
  (* This is only the case if span > 136 years (64bit system) *)
  |> CCOption.get_or ~default:"Session duration too long!"
;;
