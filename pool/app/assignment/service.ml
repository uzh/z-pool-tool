open Entity

let src = Logs.Src.create "session_reminder.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith

let create_reminders pool tenant sys_languages session experiment =
  let open Utils.Lwt_result.Infix in
  let* assignments = Repo.find_uncanceled_by_session pool session.Session.id in
  let%lwt create_message =
    Message_template.SessionReminder.prepare
      pool
      tenant
      sys_languages
      experiment
      session
  in
  let emails =
    CCList.map
      (fun (assignment : t) ->
        let contact = assignment.contact in
        create_message contact)
      assignments
  in
  emails |> CCResult.flatten_l |> Lwt_result.lift
;;

let create_events data =
  data
  |> CCList.fold_left
       (fun (session_events, emails)
            (session, { Experiment.smtp_auth_id; _ }, reminders) ->
         let reminders =
           reminders |> CCList.map (fun email -> email, smtp_auth_id)
         in
         Session.ReminderSent session :: session_events, reminders @ emails)
       ([], [])
  |> fun (session_events, emails) -> session_events, Email.BulkSent emails
;;

let send_tenant_reminder ({ Pool_tenant.database_label; _ } as tenant) =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let run () =
    let* sessions = Session.find_sessions_to_remind database_label in
    let%lwt sys_languages = Settings.find_languages database_label in
    Lwt_list.map_s
      (fun session ->
        Experiment.find_of_session
          database_label
          (Session.Id.to_common session.Session.id)
        >>= fun experiment ->
        create_reminders database_label tenant sys_languages session experiment
        >|+ fun emails -> session, experiment, emails)
      sessions
    ||> CCResult.flatten_l
    >|+ create_events
    |>> fun (session_events, email_event) ->
    let%lwt () = Email.handle_event database_label email_event in
    let%lwt () =
      Lwt_list.iter_s (Session.handle_event database_label) session_events
    in
    Lwt.return sessions
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
  | Ok sessions ->
    (match sessions with
     | [] -> ()
     | sessions ->
       Logs.info ~src (fun m ->
         m
           "Reminder sent for the following sessions: %s"
           (sessions
            |> CCList.map (fun { Session.id; _ } -> Session.Id.value id)
            |> CCString.concat ", ")))
;;

let run () =
  let open Utils.Lwt_result.Infix in
  () |> Pool_tenant.find_all >|> Lwt_list.iter_s send_tenant_reminder
;;

let start_handler () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s 30 in
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
