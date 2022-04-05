let create pool () =
  ()
  |> Cqrs_command.Settings_command.RestoreDefault.handle
  |> function
  | Ok events -> Lwt_list.iter_s (Pool_event.handle_event pool) events
  | Error err -> failwith Pool_common.(Utils.error_to_string Language.En err)
;;
