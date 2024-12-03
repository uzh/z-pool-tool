let name = "connection-watcher"
let src = Logs.Src.create [%string "system_event.%{name}"]

let rerun_migrations_for_connection_issues () =
  let open Utils.Lwt_result.Infix in
  let open Database in
  Logs.debug ~src (fun m ->
    m ~tags:(Logger.Tags.create Pool.Root.label) "Check migrations for connection issues");
  let connect_and_migrate pool =
    Pool_database.connect_and_migrate pool
    >|- Pool_common.Utils.with_log_error ~tags:(Database.Logger.Tags.create pool)
    ||> Utils.ignore_res
  in
  Pool.Tenant.all ~status:Status.[ MigrationsConnectionIssue ] ()
  |> Lwt_list.iter_s connect_and_migrate
;;

let start () =
  let open Schedule in
  let interval =
    (if Sihl.Configuration.is_production () then 15 * 60 else 10)
    |> Ptime.Span.of_int_s
    |> ScheduledTimeSpan.of_span
  in
  create name (Every interval) None rerun_migrations_for_connection_issues
  |> add_and_start
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    name
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start
;;

let register () = Sihl.Container.Service.create lifecycle
