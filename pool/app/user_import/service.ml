let src = Logs.Src.create "user_import.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith
let interval_s = 15 * 60
let limit = 50 (* equals 4'800 emails a day *)

let run database_label =
  let open Utils.Lwt_result.Infix in
  let%lwt import_message =
    let%lwt tenant =
      Pool_tenant.find_by_label database_label ||> get_or_failwith
    in
    Message_template.UserImport.prepare database_label tenant
  in
  let%lwt admins = Repo.find_admins_to_notify database_label limit () in
  let new_limit = limit - CCList.length admins in
  let%lwt contacts =
    if new_limit > 0
    then Repo.find_contacts_to_notify database_label new_limit ()
    else Lwt.return []
  in
  let make_events (messages, events) contact import =
    let message = import_message contact import.Entity.token in
    let event = Event.Notified import in
    message :: messages, event :: events
  in
  let events =
    admins
    |> CCList.fold_left
         (fun acc (admin, import) -> make_events acc (`Admin admin) import)
         ([], [])
  in
  let messages, events =
    contacts
    |> CCList.fold_left
         (fun acc (contact, import) ->
           make_events acc (`Contact contact) import)
         events
  in
  let%lwt () = Email.BulkSent messages |> Email.handle_event database_label in
  events |> Lwt_list.iter_s (Event.handle_event database_label)
;;

let run_all () =
  let open Utils.Lwt_result.Infix in
  Pool_tenant.find_databases ()
  >|> Lwt_list.iter_s (fun { Pool_database.label; _ } -> run label)
;;

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s interval_s in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m ~tags:Pool_database.(Logger.Tags.create root) "Run");
    run_all ()
  in
  create
    "import_notifications"
    (Every (interval |> ScheduledTimeSpan.of_span))
    periodic_fcn
  |> Schedule.add_and_start
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "System events"
    ~dependencies:(fun () -> [ Database.lifecycle ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
