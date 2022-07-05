module SessionCommand = Cqrs_command.Session_command
module Field = Pool_common.Message.Field

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let reschedule_to_past () =
  let session = Test_utils.create_session () in
  let command =
    Session.
      { start =
          Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s @@ (60 * 60))
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      ; duration = session.Session.duration
      }
  in
  let events = SessionCommand.Reschedule.handle [] session [] command in
  let expected = Error Pool_common.Message.TimeInPast in
  check_result expected events
;;
