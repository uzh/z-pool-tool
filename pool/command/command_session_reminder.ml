let reminder_email sys_languages contact (session : Session.t) template =
  (* TODO[tinhub]: Sihl 4.0: add text elements to for subject *)
  let name = Contact.fullname contact in
  let email = Contact.email_address contact in
  let session_overview =
    CCList.map (fun lang ->
      ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
      , Session.(to_email_text lang session) ))
  in
  Email.Helper.prepare_boilerplate_email
    template
    (email |> Pool_user.EmailAddress.value)
    (("name", name) :: session_overview sys_languages)
;;

let create_reminders pool default_language sys_languages session =
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find_of_session pool session.Session.id in
  let custom_template layout =
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
          { subject = Subject.String subject
          ; content = Content.String text
          ; layout
          }
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
        let message_language =
          CCOption.value ~default:default_language contact.Contact.language
        in
        let* tenant = Pool_tenant.find_by_label pool in
        let layout = Email.Helper.layout_from_tenant tenant in
        match custom_template layout with
        | Some template ->
          Lwt_result.return
            (reminder_email sys_languages contact session template)
        | None ->
          (match Hashtbl.find_opt i18n_texts message_language with
           | Some template ->
             Lwt_result.return
               (reminder_email sys_languages contact session template)
           | None ->
             let find = CCFun.flip (I18n.find_by_key pool) message_language in
             let* subject = find I18n.Key.InvitationSubject in
             let* text = find I18n.Key.InvitationText in
             let template =
               Email.CustomTemplate.
                 { subject = Subject.I18n subject
                 ; content = Content.I18n text
                 ; layout
                 }
             in
             let () = Hashtbl.add i18n_texts message_language template in
             Lwt_result.return
               (reminder_email sys_languages contact session template)))
      assignments
  in
  let open Utils.Lwt_result.Infix in
  emails ||> CCResult.flatten_l
;;

let send_tenant_reminder pool =
  Utils.Lwt_result.map_error Pool_common.Utils.with_log_error
  @@
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let data =
    let* sessions = Session.find_sessions_to_remind pool in
    let%lwt sys_languages = Settings.find_languages pool in
    let* default_language =
      CCList.head_opt sys_languages
      |> CCOption.to_result Pool_common.Message.(Retrieve Field.Language)
      |> Lwt_result.lift
    in
    Lwt_list.map_s
      (fun session ->
        create_reminders pool default_language sys_languages session
        >|+ fun emails -> session, emails)
      sessions
    ||> CCResult.flatten_l
  in
  let events =
    Cqrs_command.Session_command.SendReminder.handle %> Lwt_result.lift
  in
  data >>= events |>> Pool_event.handle_events pool
;;

let tenant_specific_session_reminder =
  Sihl.Command.make
    ~name:"session_reminder.send"
    ~description:"Send session reminders of specified tenant"
    (fun args ->
    match args with
    | [ pool ] ->
      let open Utils.Lwt_result.Infix in
      let%lwt _ = Command_utils.setup_databases () in
      pool
      |> Pool_database.Label.create
      |> Lwt_result.lift
      >>= send_tenant_reminder
      ||> CCOption.of_result
    | _ -> failwith "Argument missmatch")
;;

let all_tenants_session_reminder =
  Sihl.Command.make
    ~name:"session_reminder.send_all"
    ~description:"Send session reminders of all tenants"
    (fun args ->
    match args with
    | [] ->
      let open CCFun in
      let open Utils.Lwt_result.Infix in
      Command_utils.setup_databases ()
      >|> Lwt_list.map_s (fun pool -> send_tenant_reminder pool)
      ||> CCList.all_ok
      ||> (fun _ -> Ok ()) %> CCOption.of_result
    | _ -> failwith "Argument missmatch")
;;
