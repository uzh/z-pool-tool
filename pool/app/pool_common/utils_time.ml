include Utils.Ptime

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
    >|= fun h -> h *. 60. |> CCInt.of_float |> Ptime.Span.of_int_s
;;

let parse_date str =
  let open CCOption in
  let error = Entity_message.(Invalid Field.Date) in
  let split_date_string date =
    date
    |> CCString.split_on_char '-'
    |> CCList.map CCInt.of_string
    |> sequence_l
  in
  split_date_string str
  >>= (function
         | [ y; m; d ] -> (y, m, d) |> Ptime.of_date >|= Ptime.to_date
         | _ -> None)
  |> CCOption.to_result error
;;

let parse_date_from_calendar str =
  let open CCFun.Infix in
  let open CCResult.Infix in
  str
  |> CCString.split ~by:"T"
  |> CCList.hd
  |> parse_date
  >>= Ptime.of_date %> CCOption.to_result Entity_message.(Invalid Field.Date)
;;

let start_is_before_end ~start ~end_at =
  if Ptime.is_later start ~than:end_at
  then Error Entity_message.EndBeforeStart
  else Ok ()
;;
