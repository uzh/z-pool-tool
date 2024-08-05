open CCFun.Infix
include Entity
include Service
module Logger = Logger
module Caqti_encoders = Caqti_encoders
module Migration = Migration

module Repo = struct
  include Repo_entity
  include Repo
end

let log_start label =
  Logs.info (fun m -> m ~tags:(Logger.Tags.create label) "Start database")
;;

let test_connection url =
  let uri = url |> Url.value |> Uri.of_string in
  Caqti_lwt_unix.with_connection uri (fun (_ : Caqti_lwt.connection) ->
    Lwt_result.return ())
;;

module Root = struct
  let label = root

  let add () =
    let database = database_url () |> create label in
    let tags = database |> Entity.label |> Logger.Tags.create in
    let log_db = show database in
    match add_pool ~pool_size:(pool_size ()) database with
    | Ok _ as status ->
      Logs.debug (fun m -> m ~tags "Database connected: %s" log_db);
      status
    | Error err as status ->
      Logs.warn (fun m ->
        m
          ~tags
          "Database connection failed: [%s] [%s]"
          log_db
          (Caqti_error.show err));
      status
  ;;

  let setup = add %> Lwt.return

  let start () =
    log_start label;
    let%lwt (_ : status) = setup () in
    Lwt.return_unit
  ;;

  let stop () = Lwt.return_unit
end

module Tenant = struct
  let add database =
    let label = label database in
    let tags = Logger.Tags.create label in
    let log_db = show database in
    match add_pool ~pool_size:(pool_size ()) database with
    | Ok _ ->
      Logs.debug (fun m -> m ~tags "Database connected: %s" log_db);
      Lwt.return label
    | Error err ->
      let%lwt () = Repo.update_status root label Status.ConnectionIssue in
      Logs.warn (fun m ->
        m
          ~tags
          "Database connection failed: [%s] [%s]"
          log_db
          (Caqti_error.show err));
      Lwt.return label
  ;;

  let setup () =
    let start_tenant database =
      log_start (label database);
      add database
    in
    match%lwt Repo.find_all_by_status root with
    | [] ->
      Logs.warn (fun m ->
        m "%s" Pool_message.Error.(NoTenantsRegistered |> show));
      Lwt.return []
    | tenants -> Lwt_list.map_s start_tenant tenants
  ;;

  let find = Repo.find root

  let find_all_by_status ?status () =
    Repo.find_all_by_status ?status root |> Lwt.map (CCList.map Entity.label)
  ;;

  let find_label_by_url ?allowed_status =
    Repo.find_label_by_url ?allowed_status root
  ;;

  let update_status = Repo.update_status root
  let set_migration_pending = Repo.set_migration_pending

  let start () =
    let%lwt (_ : Label.t list) = setup () in
    Lwt.return_unit
  ;;

  let stop () = Lwt.return_unit

  let test_connection database_label =
    let open Utils.Lwt_result.Infix in
    let* database = Repo.find root database_label in
    match%lwt database |> url |> test_connection with
    | Ok () -> Lwt.return_ok ()
    | Error (_ : Caqti_error.load_or_connect) ->
      let%lwt () =
        Repo.update_status root (database |> label) Status.ConnectionIssue
      in
      Lwt_result.fail Pool_message.(Error.SessionTenantNotFound)
  ;;
end

let test_and_create url label =
  match%lwt test_connection url with
  | Ok () -> create label url |> Lwt.return_ok
  | Error (_ : Caqti_error.load_or_connect) ->
    Lwt_result.fail Pool_message.(Error.Invalid Field.DatabaseUrl)
;;
