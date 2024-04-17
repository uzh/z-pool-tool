let src = Logs.Src.create "user_import.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith
let interval_s = 15 * 60
let limit = 50 (* equals 4'800 emails a day *)

let reminder_settings database_label =
  let open Settings in
  Lwt.both
    (find_user_import_first_reminder_after database_label)
    (find_user_import_second_reminder_after database_label)
;;

let message_history user =
  let open Queue.History in
  { entity_uuids = [ user.Sihl_user.id |> Pool_common.Id.of_string ]
  ; message_template =
      Message_template.Label.(show SignUpVerification) |> CCOption.return
  }
;;

let run database_label =
  let open Utils.Lwt_result.Infix in
  let%lwt import_message =
    let%lwt tenant =
      Pool_tenant.find_by_label database_label ||> get_or_failwith
    in
    Message_template.UserImport.prepare database_label tenant
  in
  let to_admin = CCList.map (fun (admin, import) -> `Admin admin, import) in
  let to_contact =
    CCList.map (fun (contact, import) -> `Contact contact, import)
  in
  let run_limit fcn decode limit = fcn database_label limit () ||> decode in
  let%lwt reminder_settings = reminder_settings database_label in
  let tasks =
    [ run_limit Repo.find_admins_to_notify to_admin, Event.notified
    ; run_limit Repo.find_contacts_to_notify to_contact, Event.notified
    ; ( run_limit (Repo.find_admins_to_remind reminder_settings) to_admin
      , Event.reminded )
    ; ( run_limit (Repo.find_contacts_to_remind reminder_settings) to_contact
      , Event.reminded )
    ]
  in
  let make_events (jobs, events) (user, import) event_fnc =
    let job = import_message user import.Entity.token in
    let event = event_fnc import in
    job :: jobs, event :: events
  in
  let rec folder limit tasks events =
    if limit <= 0
    then Lwt.return events
    else (
      match tasks with
      | [] -> Lwt.return events
      | (repo_fnc, event_fnc) :: tl ->
        let%lwt users = repo_fnc limit in
        let new_limit = limit - CCList.length users in
        CCList.fold_left
          (fun events data -> make_events events data event_fnc)
          events
          users
        |> folder new_limit tl)
  in
  let%lwt emails, import_events = folder limit tasks ([], []) in
  let%lwt () = Email.(BulkSent emails |> handle_event database_label) in
  import_events |> Lwt_list.iter_s (Event.handle_event database_label)
;;

let run_all () =
  let open Utils.Lwt_result.Infix in
  Database.Tenant.find_all_running () >|> Lwt_list.iter_s run
;;

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s interval_s in
  let periodic_fcn () =
    Logs.debug ~src (fun m -> m ~tags:Database.(Logger.Tags.create root) "Run");
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
    ~dependencies:(fun () -> [ Pool_database.lifecycle; Queue.lifecycle () ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
