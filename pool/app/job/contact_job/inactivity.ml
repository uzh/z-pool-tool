open Utils.Lwt_result.Infix

let src = Logs.Src.create "contacts.service"

let warning_notification_events pool = function
  | [] -> Lwt_result.return None
  | contacts ->
    let* tenant = Pool_tenant.find_by_label pool in
    (* TODO: deactivation_at *)
    let deactivation_at = Ptime_clock.now () in
    let%lwt make_message =
      Message_template.InactiveContactWarning.prepare tenant ~deactivation_at
    in
    let* messages = contacts |> Lwt_list.map_s make_message ||> CCList.all_ok in
    Lwt_result.return (Some messages)
;;

let run_by_tenant pool =
  let open Settings in
  let%lwt warn_after =
    Settings.find_inactive_user_warning pool ||> InactiveUser.Warning.value
  in
  (* let%lwt disable_after = Settings.find_inactive_user_warning pool in *)
  let%lwt contacts_to_warn = Contact.find_by_last_sign_earlier_than pool warn_after in
  warning_notification_events pool contacts_to_warn
  >|> function
  | Error err ->
    let open Pool_common in
    Logs.err (fun m ->
      m
        "An error occurred while trying to warn contacts about account inavtivity: %s"
        (Utils.error_to_string Language.En err));
    Utils.failwith err
  | Ok None ->
    Logs.info (fun m -> m "No contacts found to warn due to account inavtivity");
    Lwt.return_unit
  | Ok (Some messages) ->
    Logs.info (fun m ->
      m "Warning %i contacts about account inavtivity" (CCList.length messages));
    Email.BulkSent messages |> Email.handle_event pool
;;

let run () =
  Database.(Pool.Tenant.all ~status:[ Status.Active ] ()) |> Lwt_list.iter_s run_by_tenant
;;

let start_handler () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (60 * 60 * 4) in
  let periodic_fcn () =
    Logs.debug ~src (fun m -> m ~tags:Database.(Logger.Tags.create Pool.Root.label) "Run");
    run ()
  in
  create
    "notify_inactive_contacts"
    (Every (ScheduledTimeSpan.of_span interval))
    None
    periodic_fcn
  |> Schedule.add_and_start
;;

let start () =
  let open Sihl.Configuration in
  match
    CCOption.get_or ~default:false (read_bool "RUN_HANDLE_INACTIVE_CONTCATS")
    || is_production ()
  with
  | true -> start_handler ()
  | false -> Lwt.return_unit
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "Inactive contacts"
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
