let delete database_label template_id redirect =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Message_template_command.Delete in
  template_id
  |> Message_template.find database_label
  >== handle
  |>> Pool_event.handle_events database_label
  |>> (fun () ->
        let open Http_utils in
        redirect_to_with_actions
          redirect
          [ Message.set
              ~success:[ Pool_message.(Success.Deleted Field.MessageTemplate) ]
          ])
  >|- fun err -> err, redirect
;;
