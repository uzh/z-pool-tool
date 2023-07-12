let get_or_failwith = Pool_common.Utils.get_or_failwith
let src = Logs.Src.create "command.session_reminder"

let create_reminders pool tenant sys_languages session experiment =
  let open Utils.Lwt_result.Infix in
  let* assignments =
    Assignment.find_uncanceled_by_session pool session.Session.id
  in
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
      (fun (assignment : Assignment.t) ->
        let contact = assignment.Assignment.contact in
        create_message contact)
      assignments
  in
  emails |> CCResult.flatten_l |> Lwt_result.lift
;;

let send_tenant_reminder pool =
  Utils.Lwt_result.map_error
    (Pool_common.Utils.with_log_error
       ~src
       ~tags:(Pool_database.Logger.Tags.create pool))
  @@
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let data =
    let* tenant = Pool_tenant.find_by_label pool in
    let* sessions = Session.find_sessions_to_remind pool in
    let%lwt sys_languages = Settings.find_languages pool in
    Lwt_list.map_s
      (fun session ->
        Experiment.find_of_session
          pool
          (Session.Id.to_common session.Session.id)
        >>= fun experiment ->
        create_reminders pool tenant sys_languages session experiment
        >|+ fun emails -> session, experiment, emails)
      sessions
    ||> CCResult.flatten_l
  in
  let events =
    Cqrs_command.Session_command.SendReminder.handle %> Lwt_result.lift
  in
  data >>= events |>> Pool_event.handle_events pool
;;

let tenant_specific_session_reminder =
  Command_utils.make_pool_specific
    "session_reminder.send"
    "Send session reminders of specified tenant"
    (fun pool ->
    let open Utils.Lwt_result.Infix in
    pool |> send_tenant_reminder ||> get_or_failwith ||> CCOption.some)
;;

let all_tenants_session_reminder =
  Command_utils.make_no_args
    "session_reminder.send_all"
    "Send session reminders of all tenants"
    (fun () ->
    let open Utils.Lwt_result.Infix in
    Command_utils.setup_databases ()
    >|> Lwt_list.map_s (fun pool -> send_tenant_reminder pool)
    ||> CCList.all_ok
    ||> get_or_failwith
    ||> fun (_ : unit list) -> Some ())
;;
