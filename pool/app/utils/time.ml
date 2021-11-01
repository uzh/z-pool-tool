let ptime_of_date_string date =
  let date =
    date
    |> String.split_on_char '.'
    |> CCList.map int_of_string_opt
    |> CCOpt.sequence_l
    |> CCOpt.to_result
         "Invalid date string provided, make sure that day, month and year are \
          ints"
  in
  let message = "Invalid date provided, only format 01.12.1990 is accepted" in
  match date with
  | Ok [ day; month; year ] ->
    Ptime.of_date (year, month, day) |> CCOpt.to_result message
  | Ok _ -> Error message
  | Error msg -> Error msg
;;

let decimal n =
  if n < 10 then Format.asprintf "0%d" n else Format.asprintf "%d" n
;;

let formatted_date date =
  let year, month, day = date in
  Format.asprintf "%s.%s.%d" (decimal day) (decimal month) year
;;

let formatted_time time =
  let (h, m, s), _ = time in
  Format.asprintf "%s:%s:%s" (decimal h) (decimal m) (decimal s)
;;

let ptime_to_date_human ptime =
  let year, month, day = Ptime.to_date ptime in
  formatted_date (year, month, day)
;;

let ptime_to_time_human ptime =
  let _, time = Ptime.to_date_time ptime in
  formatted_time time
;;

let ptime_to_datetime_human ptime =
  let date, time = Ptime.to_date_time ptime in
  Format.asprintf "%s %s" (formatted_date date) (formatted_time time)
;;

let ptime_to_datetime_filename ptime =
  let (year, month, day), ((h, m, s), _) = Ptime.to_date_time ptime in
  Format.asprintf
    "%d%s%s-%s%s%s"
    year
    (decimal month)
    (decimal day)
    (decimal h)
    (decimal m)
    (decimal s)
;;
