let default_reminder_lead_time () =
  14400
  |> Ptime.Span.of_int_s
  |> Pool_common.Reminder.LeadTime.create
  |> CCResult.map_err (fun err ->
         Pool_common.(Utils.error_to_string Language.En err))
  |> CCResult.get_or_failwith
;;

let create_reminder language contact (session : Session.t) content subject =
  (* TODO[tinhub]: Sihl 4.0: add text elements to for subject *)
  let name = Contact.fullname contact in
  let email = Contact.email_address contact in
  let session_overview = Session.(to_email_text language session) in
  Email.Helper.prepare_boilerplate_email
    subject
    (email |> Pool_user.EmailAddress.value)
    content
    [ "name", name; "sessionOverview", session_overview ]
;;

let create_reminders pool session default_language sys_languages =
  let open Lwt_result.Syntax in
  let* experiment = Experiment.find_of_session pool session.Session.id in
  let custom_reminder_text =
    let open CCOption in
    session.Session.reminder_text
    <+> experiment.Experiment.session_reminder_text
  in
  let custom_reminder_subject =
    let open CCOption in
    session.Session.reminder_subject
    <+> experiment.Experiment.session_reminder_subject
  in
  let default_texts =
    Hashtbl.create ~random:true (CCList.length sys_languages)
  in
  let default_subjects =
    Hashtbl.create ~random:true (CCList.length sys_languages)
  in
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
        let custom_or_default value str_fnc hash i18n_key =
          match value with
          | Some value -> Lwt_result.return (str_fnc value)
          | None ->
            (match Hashtbl.find_opt hash message_language with
            | Some text -> Lwt_result.return I18n.(text |> content_to_string)
            | None ->
              let* i18n_text =
                I18n.(find_by_key pool i18n_key message_language)
              in
              let _ = Hashtbl.add hash message_language i18n_text in
              Lwt_result.return I18n.(i18n_text |> content_to_string))
        in
        let* text =
          custom_or_default
            custom_reminder_text
            Pool_common.Reminder.Text.value
            default_texts
            I18n.Key.ReminderText
        in
        let* subject =
          custom_or_default
            custom_reminder_subject
            Pool_common.Reminder.Subject.value
            default_subjects
            I18n.Key.ReminderSubject
        in
        Lwt_result.ok
          (create_reminder message_language contact session text subject))
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
            let* sessions =
              Session.find_sessions_to_remind
                pool
                (default_reminder_lead_time ())
            in
            let sys_languages = Pool_common.Language.all in
            let* default_language = Settings.default_language pool in
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
