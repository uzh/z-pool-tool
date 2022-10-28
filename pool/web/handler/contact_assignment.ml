module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    HttpUtils.(
      ( get_field_router_param req Experiment |> Pool_common.Id.of_string
      , get_field_router_param req Session |> Pool_common.Id.of_string ))
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Pool_common.Id.value)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let open Lwt_result.Syntax in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let* experiment = Experiment.find_public tenant_db experiment_id contact in
    let* session = Session.find_public tenant_db id in
    let* waiting_list =
      Waiting_list.find_by_contact_and_experiment tenant_db contact experiment
    in
    let* confirmation_email =
      let* language =
        let* default = Settings.default_language tenant_db in
        contact.Contact.language |> CCOption.value ~default |> Lwt_result.return
      in
      let* subject =
        I18n.find_by_key tenant_db I18n.Key.ConfirmationSubject language
        >|= I18n.content
      in
      let* text =
        I18n.find_by_key tenant_db I18n.Key.ConfirmationText language
        >|= I18n.content
      in
      let session_text = Session.(public_to_email_text language session) in
      Lwt_result.return Email.{ subject; text; language; session_text }
    in
    let%lwt already_enrolled =
      let open Lwt.Infix in
      Assignment.find_by_experiment_and_contact_opt
        tenant_db
        experiment.Experiment.Public.id
        contact
      >|= CCOption.is_some
    in
    let events =
      Cqrs_command.Assignment_command.Create.(
        handle
          { contact; session; waiting_list; experiment }
          confirmation_email
          already_enrolled)
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set
            ~success:[ Pool_common.Message.(AssignmentCreated) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
