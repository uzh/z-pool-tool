include Pool_core.Time

(* Parsing *)
let parse_time str =
  let open CCResult in
  Ptime.of_rfc3339 str
  |> Ptime.rfc3339_error_to_msg
  |> CCResult.map_err (fun (`Msg e) -> Pool_message.Error.NotADatetime (str, e))
  >|= fun (time, _, _) -> time
;;

let parse_time_span str =
  let error = Pool_message.(Error.Invalid Field.Duration) in
  if CCString.is_empty str
  then Error Pool_message.Error.NoValue
  else
    let open CCResult.Infix in
    str
    |> CCFloat.of_string_opt
    |> CCOption.to_result error
    >|= fun h -> h *. 60. |> CCInt.of_float |> Ptime.Span.of_int_s
;;

module Date = struct
  include Pool_core.Time.Date

  let of_string str =
    let open CCOption in
    let error = Pool_message.(Error.Invalid Field.Date) in
    let split_date_string date =
      date |> CCString.split_on_char '-' |> CCList.map CCInt.of_string |> sequence_l
    in
    split_date_string str
    >>= (function
     | [ y; m; d ] -> (y, m, d) |> Ptime.of_date >|= Ptime.to_date
     | _ -> None)
    |> CCOption.to_result error
  ;;
end

let parse_date_from_calendar str =
  let open CCFun.Infix in
  let open CCResult.Infix in
  (* FullCalendar sends range bounds as ISO 8601 with the browser's UTC offset;
     use the exact instant. Date-only strings fall back to midnight UTC. *)
  match Ptime.of_rfc3339 str with
  | Ok (time, _, _) -> Ok time
  | Error _ ->
    str
    |> CCString.split ~by:"T"
    |> CCList.hd
    |> Date.of_string
    >>= Ptime.of_date %> CCOption.to_result Pool_message.(Error.Invalid Field.Date)
;;

let start_is_before_end ~start ~end_at =
  if Ptime.is_later start ~than:end_at
  then Error Pool_message.Error.EndBeforeStart
  else Ok ()
;;

(* The conformist schemas live here rather than in [Pool_core.Time] because
   of the [Pool_conformist]/[Pool_message] dependency. *)
let schema field create () : (Pool_message.Error.t, t) Pool_conformist.Field.t =
  let decode str = CCResult.(parse_time str >>= create) in
  let encode time = to_rfc3339 time in
  Pool_conformist.schema_decoder decode encode field
;;

module Span = struct
  include Pool_core.Time.Span

  (* Encode as total seconds; spans beyond [int] range (> 136 years on 64-bit)
     cannot occur for form values. *)
  let encode_s span = span |> to_int_s |> CCOption.map_or ~default:"" CCInt.to_string

  let schema field create () : (Pool_message.Error.t, t) Pool_conformist.Field.t =
    let open CCResult in
    let decode str = parse_time_span str >>= create in
    Pool_conformist.schema_decoder decode encode_s field
  ;;
end
