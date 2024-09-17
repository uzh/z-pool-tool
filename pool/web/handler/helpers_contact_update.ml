let tags = Pool_context.Logger.Tags.req
let src = Logs.Src.create "handler.contact.helpers_contact_update"

let toggle_paused
  { Pool_context.database_label; query_language; user; _ }
  redirect_path
  contact
  tags
  =
  let open Utils.Lwt_result.Infix in
  let open Pool_user in
  let paused = contact.Contact.paused |> Paused.value |> not |> Paused.create in
  let events =
    Cqrs_command.Contact_command.TogglePaused.handle ~tags contact paused
    |> Lwt_result.lift
  in
  let handle events =
    events |> Pool_event.handle_events ~tags database_label user
  in
  let redirect () =
    let open Http_utils in
    redirect_to_with_actions
      redirect_path
      [ Message.set
          ~success:[ Pool_message.Success.PausedToggled (Paused.value paused) ]
      ]
  in
  events
  |>> handle
  |>> redirect
  |> Utils.Lwt_result.map_error (fun err ->
    Http_utils.(err, path_with_language query_language redirect_path))
;;
