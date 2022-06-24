let default_reminder_lead_time () =
  14400
  |> Ptime.Span.of_int_s
  |> Pool_common.Reminder.LeadTime.create
  |> CCResult.map_err (fun err ->
         Pool_common.(Utils.error_to_string Language.En err))
  |> CCResult.get_or_failwith
;;

type reminder_text =
  | I18nText of I18n.t
  | SpecificText of Pool_common.Reminder.Text.t

let create_reminder pool language contact content subject =
  let name = Contact.fullname contact in
  let email = Contact.email_address contact in
  let subject = I18n.content_to_string subject in
  let content =
    match content with
    | I18nText text -> I18n.content_to_string text
    | SpecificText text -> Pool_common.Reminder.Text.value text
  in
  Email.Helper.prepare_email
    pool
    language
    Email.TemplateLabel.SessionReminder
    subject
    (email |> Pool_user.EmailAddress.value)
    [ "name", name; "textContent", content ]
;;

let create_reminders pool session default_language sys_languages =
  let open Lwt_result.Syntax in
  let* experiment = Experiment.find_of_session pool session.Session.id in
  (* Find custom reminder text, if available *)
  let custom_reminder_text =
    let open CCOption in
    session.Session.reminder_text
    <+> experiment.Experiment.session_reminder_text
  in
  let default_texts =
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
        let* text, subject =
          CCOption.map_lazy
            (fun _ ->
              match Hashtbl.find_opt default_texts message_language with
              | Some (text, subject) ->
                Ok (I18nText text, subject) |> Lwt_result.lift
              | None ->
                let* i18n_text =
                  I18n.(find_by_key pool Key.ReminderText message_language)
                in
                let* subject =
                  I18n.(find_by_key pool Key.ReminderSubject message_language)
                in
                let _ =
                  Hashtbl.add default_texts message_language (i18n_text, subject)
                in
                (I18nText i18n_text, subject) |> Lwt_result.return)
            (fun specific_text ->
              let* subject =
                I18n.(find_by_key pool Key.ReminderSubject message_language)
              in
              Ok (SpecificText specific_text, subject) |> Lwt_result.lift)
            custom_reminder_text
        in
        Lwt_result.ok
        @@ create_reminder pool message_language contact text subject)
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
