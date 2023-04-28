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
    >|= fun h -> h *. 3600. |> CCInt.of_float |> Ptime.Span.of_int_s
;;
