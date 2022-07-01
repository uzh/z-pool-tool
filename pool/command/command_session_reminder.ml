let create_reminder sys_languages contact (session : Session.t) template =
  (* TODO[tinhub]: Sihl 4.0: add text elements to for subject *)
  let name = Contact.fullname contact in
  let email = Contact.email_address contact in
  let session_overview =
    (CCList.map (fun lang ->
         ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
         , Session.(to_email_text lang session) )))
      sys_languages
  in
  Email.Helper.prepare_boilerplate_email
    template
    (email |> Pool_user.EmailAddress.value)
    (("name", name) :: session_overview)
;;

let create_reminders pool session default_language sys_languages =
  let open Lwt_result.Syntax in
  let* experiment = Experiment.find_of_session pool session.Session.id in
  let custom_template =
    let open CCOption in
    let custom_reminder_text =
      session.Session.reminder_text
      <+> experiment.Experiment.session_reminder_text
    in
    let custom_reminder_subject =
      session.Session.reminder_subject
      <+> experiment.Experiment.session_reminder_subject
    in
    match custom_reminder_text, custom_reminder_subject with
    | Some text, Some subject ->
      let open Pool_common.Reminder in
      let subject = Subject.value subject in
      let text = Text.value text in
      Some
        Email.CustomTemplate.
          { subject = Subject.String subject; content = Content.String text }
    | _ -> None
  in
  let i18n_texts = Hashtbl.create ~random:true (CCList.length sys_languages) in
  let* assignments =
    Assignment.find_uncanceled_by_session pool session.Session.id
  in
  let emails =
    Lwt_list.map_s
      (fun (assignment : Assignment.t) ->
        let contact = assignment.Assignment.contact in
        match custom_template with
        | Some template ->
          Lwt_result.ok (create_reminder sys_languages contact session template)
        | None ->
          let message_language =
            CCOption.value ~default:default_language contact.Contact.language
          in
          (match Hashtbl.find_opt i18n_texts message_language with
          | Some template ->
            Lwt_result.ok
              (create_reminder sys_languages contact session template)
          | None ->
            let* subject =
              I18n.(find_by_key pool Key.InvitationSubject message_language)
            in
            let* text =
              I18n.(find_by_key pool Key.InvitationText message_language)
            in
            let template =
              Email.CustomTemplate.
                { subject = Subject.I18n subject; content = Content.I18n text }
            in
            let () = Hashtbl.add i18n_texts message_language template in
            Lwt_result.ok
              (create_reminder sys_languages contact session template)))
      assignments
  in
  let open Utils.Lwt_result.Infix in
  emails ||> CCResult.flatten_l
;;

let send_session_reminder =
  Sihl.Command.make
    ~name:"session_reminder.send"
    ~description:"Send session reminders"
    (fun args ->
      match args with
      | [ pool ] ->
        let open Lwt_result.Syntax in
        let open Utils.Lwt_result.Infix in
        let%lwt _ = Database.Tenant.setup () in
        let%lwt result =
          let* pool = pool |> Pool_database.Label.create |> Lwt_result.lift in
          let%lwt data =
            let%lwt default_reminder_lead_time =
              Settings.find_default_reminder_lead_time pool
            in
            let* sessions =
              Session.find_sessions_to_remind pool default_reminder_lead_time
            in
            let%lwt sys_languages = Settings.find_languages pool in
            let* default_language =
              CCList.head_opt sys_languages
              |> CCOption.to_result
                   Pool_common.Message.(Retrieve Field.Language)
              |> Lwt_result.lift
            in
            Lwt_list.map_s
              (fun session ->
                let* emails =
                  create_reminders pool session default_language sys_languages
                in
                Lwt_result.return (session, emails))
              sessions
            ||> CCResult.flatten_l
          in
          let* events =
            let open CCResult.Infix in
            data
            >>= Cqrs_command.Session_command.SendReminder.handle
            |> Lwt_result.lift
          in
          Pool_event.handle_events pool events |> Lwt_result.return
        in
        (match result with
        | Ok _ -> Lwt.return_some ()
        | Error _ -> Lwt.return_none)
      | _ -> failwith "Argument missmatch")
;;
