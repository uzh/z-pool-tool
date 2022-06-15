let tz_offset_s ?(hours = 2) () = 3600 * hours

let validate_date_ints (year, month, day) =
  let valid i min max = i >= min && i <= max in
  match valid year 1000 9999 && valid month 1 12 && valid day 1 31 with
  | true -> Ok (year, month, day)
  | false -> Error Entity_message.(Invalid Field.Date)
;;

let split_date_string date =
  date
  |> CCString.split_on_char '.'
  |> CCList.map int_of_string_opt
  |> CCOption.sequence_l
  |> CCOption.to_result Entity_message.(Invalid Field.Date)
;;

let ptime_of_date_string date =
  let open CCResult.Infix in
  let date = split_date_string date in
  match date with
  | Ok [ day; month; year ] ->
    (year, month, day)
    |> validate_date_ints
    >>= fun date ->
    date
    |> Ptime.of_date
    |> CCOption.to_result Entity_message.(Invalid Field.Date)
  | Ok _ -> Error Entity_message.(Invalid Field.Date)
  | Error msg -> Error msg
;;

let ptime_date_of_date_string date =
  let date = split_date_string date in
  match date with
  | Ok [ day; month; year ] -> (year, month, day) |> validate_date_ints
  | Ok _ -> Error Entity_message.(Invalid Field.DateTime)
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
  let _, ((h, m, s), _) = time in
  Format.asprintf "%s:%s:%s" (decimal h) (decimal m) (decimal s)
;;

let formatted_date_time (date : Ptime.t) =
  Format.asprintf
    "%s %s"
    (formatted_date (Ptime.to_date date))
    (formatted_time (Ptime.to_date_time ~tz_offset_s:(tz_offset_s ()) date))
;;

let ptime_to_sexp p =
  let formatted = p |> formatted_date_time in
  Sexplib0.Sexp.Atom formatted
;;

let%test "create valid dates from string" =
  let valid_dates =
    [ "01.01.1990"; "5.12.2022"; "1.1.2022"; "5.01.2000"; "12.1.2100" ]
  in
  let all = CCList.map ptime_date_of_date_string valid_dates |> CCList.all_ok in
  match all with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "create invalid dates from string" =
  let invalid_dates =
    [ "12"; "01.01"; "05.2020"; "120.01.2020"; "01.13.2020"; "01.01.20" ]
  in
  CCList.for_all
    (fun iban ->
      match ptime_date_of_date_string iban with
      | Ok _ -> false
      | Error _ -> true)
    invalid_dates
;;

let formatted_timespan timespan =
  Ptime.Span.pp Format.str_formatter timespan;
  Format.flush_str_formatter ()
;;

let timespan_spanpicker timespan =
  timespan
  |> Ptime.Span.to_int_s
  |> CCOption.map_or ~default:"" (fun timespan ->
         let h = timespan / 3600 in
         let timespan = timespan - (h * 3600) in
         let min = timespan / 60 in
         let timespan = timespan - (min * 60) in
         let s = timespan in
         Format.asprintf
           "%s:%s:%s"
           (h |> decimal)
           (min |> decimal)
           (s |> decimal))
;;
