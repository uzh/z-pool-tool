let handle_system_events_command =
  Command_utils.make_no_args
    "system_event.handle"
    "Handle system events on one host"
    (fun () ->
      let open Utils.Lwt_result.Infix in
      System_event.Service.run () ||> CCOption.return)
;;
