let handle_system_events_command =
  Command_utils.make_no_args
    "system_event.handle"
    "Handle system events on one host"
    (fun () ->
       let open Utils.Lwt_result.Infix in
       let%lwt (_ : Database.Label.t list) = Command_utils.setup_databases () in
       System_event.Service.run `Server () ||> CCOption.return)
;;
