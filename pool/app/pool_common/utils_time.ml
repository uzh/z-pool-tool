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
  let open CCOption in
  let open CCFun in
  Ptime_clock.current_tz_offset_s ()
  >>= Ptime.Span.of_int_s %> Ptime.add_span date
  |> value ~default:date
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

let yojson_of_ptime_span m =
  m
  |> Ptime.Span.to_int_s
  |> CCOption.map (fun i -> i |> CCInt.to_string |> Yojson.Safe.from_string)
  |> CCOption.get_exn_or "Invalid timespan provided"
;;

(* Parsing *)
let parse_time str =
  let open CCResult in
  Ptime.of_rfc3339 str
  |> Ptime.rfc3339_error_to_msg
  |> CCResult.map_err (fun (`Msg e) -> Entity_message.NotADatetime (str, e))
  >|= fun (time, _, _) -> time
;;

let parse_time_span str =
  let error = Entity_message.(Invalid Field.Duration) in
  if CCString.is_empty str
  then Error Entity_message.NoValue
  else
    let open CCResult.Infix in
    str
    |> CCFloat.of_string_opt
    |> CCOption.to_result error
    >|= fun h -> h *. 3600. |> CCInt.of_float |> Ptime.Span.of_int_s
;;

let print_time_span span =
  Ptime.Span.to_int_s span
  |> CCOption.map CCInt.to_string
  (* If span is bigger than int max (signed), print an info string instead *)
  (* This is only the case if span > 136 years (64bit system) *)
  |> CCOption.get_or ~default:"Session duration too long!"
;;
