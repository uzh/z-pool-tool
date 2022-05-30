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
    [ "name", name; "content", content ]
;;

let send_reminder pool session =
  let open Lwt_result.Syntax in
  let* experiment = Experiment.find_of_session pool session.Session.id in
  let specific_reminder_text =
    match session.Session.reminder_text with
    | Some text -> Some text
    | None -> experiment.Experiment.session_reminder_text
  in
  let sys_languages = Pool_common.Language.all () in
  let (default_texts : (Pool_common.Language.t, I18n.t * I18n.t) Hashtbl.t) =
    Hashtbl.create ~random:true (CCList.length sys_languages)
  in
  let* assignments =
    Assignment.find_uncanceled_by_session pool session.Session.id
  in
  let* default_language = Settings.default_language pool in
  let%lwt emails =
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
              (* TODO [timhub]: how to determine subject of customized text?
                 Which language is it saved in? *)
              let* subject =
                I18n.(find_by_key pool Key.ReminderSubject message_language)
              in
              Ok (SpecificText specific_text, subject) |> Lwt_result.lift)
            specific_reminder_text
        in
        Lwt_result.ok
        @@ create_reminder pool message_language contact text subject)
      assignments
  in
  let open CCResult.Infix in
  emails
  |> CCResult.flatten_l
  >|= Service.Email.bulk_send ~ctx:(Pool_tenant.to_ctx pool)
  |> Lwt_result.lift
;;

let test_reminder =
  Sihl.Command.make
    ~name:"reminder.test"
    ~description:"Test reminder"
    (fun args ->
      match args with
      | [ session_id ] ->
        let open Lwt_result.Syntax in
        let%lwt result =
          let session_id = Pool_common.Id.of_string session_id in
          let%lwt selections = Pool_tenant.Selection.find_all () in
          let* pool =
            CCList.assoc_opt
              ~eq:(fun url label -> CCString.prefix ~pre:url label)
              "localhost:3017"
              (selections
              |> CCList.map (fun sel ->
                     Pool_tenant.Selection.(url sel, label sel)))
            |> CCOption.to_result
                 Pool_common.Message.(NotFound Field.TenantPool)
            |> CCResult.map_err
                 (CCFun.const Pool_common.Message.SessionTenantNotFound)
            |> Lwt_result.lift
          in
          Logs.info (fun m -> m "%s" (pool |> Pool_database.Label.value));
          let* session = Session.find pool session_id in
          send_reminder pool session
        in
        (match result with
        | Ok _ -> Lwt.return_some ()
        | Error _ -> failwith "Something went wrong")
      | _ -> failwith "Arguments do not match")
;;
