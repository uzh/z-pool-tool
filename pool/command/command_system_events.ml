let get_or_failwith = Pool_common.Utils.get_or_failwith

let handle_system_events () =
  let open System_event in
  let open Utils.Lwt_result.Infix in
  ()
  |> EventLog.ServiceIdentifier.get
  |> find_pending
  >|> Lwt_list.iter_s handle_system_event
;;

let handle_system_events_command =
  Command_utils.make_no_args
    "system_event.handle"
    "Handle system events on one host"
    (fun () ->
    let open Utils.Lwt_result.Infix in
    (* Is this required? *)
    let%lwt () = Database.Root.setup () in
    handle_system_events () ||> CCOption.return)
;;
