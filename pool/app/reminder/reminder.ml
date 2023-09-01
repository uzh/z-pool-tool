open Assignment

let src = Logs.Src.create "session_reminder.service"

let create_reminder_emails pool tenant sys_languages session experiment =
  let open Utils.Lwt_result.Infix in
  let* assignments = find_uncanceled_by_session pool session.Session.id in
  let%lwt create_message =
    Message_template.SessionReminder.prepare_emails
      pool
      tenant
      sys_languages
      experiment
      session
  in
  let emails = CCList.map create_message assignments in
  emails |> CCResult.flatten_l |> Lwt_result.lift
;;

let create_reminder_text_messages pool tenant sys_languages session experiment =
  let open Utils.Lwt_result.Infix in
  let* assignments = find_uncanceled_by_session pool session.Session.id in
  let%lwt create_message =
    Message_template.SessionReminder.prepare_text_messages
      pool
      tenant
      sys_languages
      experiment
      session
  in
  let messages =
    assignments
    |> CCList.filter_map (fun ({ contact; _ } as assignments) ->
      match contact.Contact.cell_phone with
      | None -> None
      | Some cell_phone -> Some (create_message assignments cell_phone))
  in
  messages |> CCResult.flatten_l |> Lwt_result.lift
;;

let create_email_events data =
  data
  |> CCList.fold_left
       (fun (session_events, emails)
            (session, { Experiment.smtp_auth_id; _ }, reminders) ->
         let reminders =
           reminders |> CCList.map (fun email -> email, smtp_auth_id)
         in
         ( (Session.EmailReminderSent session |> Pool_event.session)
           :: session_events
         , reminders @ emails ))
       ([], [])
  |> fun (session_events, emails) ->
  (Email.BulkSent emails |> Pool_event.email) :: session_events
;;

let create_text_message_events data =
  data
  |> CCList.fold_left
       (fun (session_events, text_messages) (session, reminders) ->
         ( (Session.TextMsgReminderSent session |> Pool_event.session)
           :: session_events
         , reminders @ text_messages ))
       ([], [])
  |> fun (session_events, text_messages) ->
  (Text_message.BulkSent text_messages |> Pool_event.text_message)
  :: session_events
;;

let create_reminder_events
  ({ Pool_tenant.database_label; _ } as tenant)
  email_reminders
  text_message_reminders
  =
  let open Utils.Lwt_result.Infix in
  let%lwt sys_languages = Settings.find_languages database_label in
  let* email_events =
    Lwt_list.map_s
      (fun session ->
        Experiment.find_of_session
          database_label
          (Session.Id.to_common session.Session.id)
        >>= fun experiment ->
        create_reminder_emails
          database_label
          tenant
          sys_languages
          session
          experiment
        >|+ fun emails -> session, experiment, emails)
      email_reminders
    ||> CCResult.flatten_l
    >|+ create_email_events
  in
  let* text_msg_events =
    Lwt_list.map_s
      (fun session ->
        Experiment.find_of_session
          database_label
          (Session.Id.to_common session.Session.id)
        >>= fun experiment ->
        create_reminder_text_messages
          database_label
          tenant
          sys_languages
          session
          experiment
        >|+ fun emails -> session, emails)
      text_message_reminders
    ||> CCResult.flatten_l
    >|+ create_text_message_events
  in
  Lwt_result.return (email_events @ text_msg_events)
;;

let send_tenant_reminder ({ Pool_tenant.database_label; _ } as tenant) =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let run () =
    let* email_reminders, text_message_reminders =
      Session.find_sessions_to_remind database_label
    in
    let* events =
      create_reminder_events tenant email_reminders text_message_reminders
    in
    let%lwt () = Pool_event.handle_events database_label events in
    Lwt_result.return (email_reminders, text_message_reminders)
  in
  ()
  |> run
  ||> function
  | Error err ->
    Logs.err ~src (fun m ->
      m
        ~tags:Pool_database.(Logger.Tags.create root)
        "Serialized message string was NULL, can not deserialize message. \
         Please fix the string manually and reset the job instance. Error: %s"
        Pool_common.(Utils.error_to_string Language.En err))
  | Ok (email_sessions, text_message_sessions) ->
    let get_ids sessions =
      sessions
      |> CCList.map (fun { Session.id; _ } -> Session.Id.value id)
      |> CCString.concat ", "
    in
    [ email_sessions, "Email reminders"
    ; text_message_sessions, "Text message reminders"
    ]
    |> CCList.iter (fun (sessions, label) ->
      match sessions with
      | [] -> ()
      | sessions ->
        Logs.info (fun m ->
          m "%s sent for the following sessions: %s" label (sessions |> get_ids)))
;;

let run () =
  let open Utils.Lwt_result.Infix in
  () |> Pool_tenant.find_all >|> Lwt_list.iter_s send_tenant_reminder
;;

let start_handler () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s 60 in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m ~tags:Pool_database.(Logger.Tags.create root) "Run");
    run ()
  in
  create
    "session_reminder"
    (Every (interval |> ScheduledTimeSpan.of_span))
    periodic_fcn
  |> Schedule.add_and_start
;;

let start () =
  let open Sihl.Configuration in
  match
    CCOption.get_or ~default:false (read_bool "RUN_SESSION_REMINDER")
    || is_production ()
  with
  | true -> start_handler ()
  | false -> Lwt.return_unit
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "System events"
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
