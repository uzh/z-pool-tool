open Utils.Lwt_result.Infix
module Repo = Contact_job_repo

let src = Logs.Src.create "contacts.service"

let handle_disable_contacts pool disable_after warn_after =
  let* make_message = Message_template.InactiveContactDeactivation.prepare pool in
  let%lwt contacts =
    Repo.find_to_disable
      pool
      (Settings.InactiveUser.DisableAfter.value disable_after)
      (CCList.length warn_after)
  in
  let* messages, events =
    let rec make_events (messages, events) = function
      | [] -> Lwt_result.return (messages, events)
      | contact :: tl ->
        make_message contact
        |> Lwt_result.lift
        >>= fun message ->
        let contact =
          Contact.
            { contact with
              paused = Pool_user.Paused.create true
            ; paused_version = Pool_common.Version.increment contact.paused_version
            }
        in
        make_events (messages @ [ message ], events @ [ Contact.Updated contact ]) tl
    in
    make_events ([], []) contacts
  in
  Lwt_result.return (messages, events)
;;

let warning_notification_events pool = function
  | [] ->
    Logs.info ~src (fun m -> m "No 'warning after' timestamp defined.");
    Lwt_result.return ([], [])
  | contacts ->
    let* make_message = Message_template.InactiveContactWarning.prepare pool in
    let* messages, events =
      let rec make_events (messages, events) = function
        | [] -> Lwt_result.return (messages, events)
        | hd :: tl ->
          make_message hd
          >>= fun message ->
          make_events
            (messages @ [ message ], events @ [ Contact.NotifiedAbountInactivity hd ])
            tl
      in
      make_events ([], []) contacts
    in
    Lwt_result.return (messages, events)
;;

let handle_contact_warnings pool warn_after =
  let%lwt contacts_to_warn = warn_after |> Repo.find_to_warn_about_inactivity pool in
  warning_notification_events pool contacts_to_warn
;;

let handle_events pool message = function
  | Error err ->
    let open Pool_common in
    Logs.err ~src (fun m ->
      m
        "%s: An error occurred while making events: %s"
        message
        (Utils.error_to_string Language.En err));
    Utils.failwith err
  | Ok (emails, events) ->
    Logs.info ~src (fun m ->
      m "%s: Found %i contacts to notify due to inactiviey" message (CCList.length events));
    let%lwt () = Email.handle_event pool (Email.BulkSent emails) in
    events |> Lwt_list.iter_s (Contact.handle_event pool)
;;

let run_by_tenant pool =
  let open Settings in
  let%lwt disable_after = find_inactive_user_disable_after pool in
  let%lwt warn_after =
    find_inactive_user_warning pool ||> CCList.map InactiveUser.Warning.TimeSpan.value
  in
  let%lwt () =
    handle_disable_contacts pool disable_after warn_after
    >|> handle_events pool "Pausing inactive users:"
  in
  let%lwt () =
    handle_contact_warnings pool warn_after
    >|> handle_events pool "Notify inactive users:"
  in
  Lwt.return_unit
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
