open CCFun.Infix
include Entity
include Service
module Logger = Logger
module Caqti_encoders = Caqti_encoders
module Migration = Migration
module Repo = Repo

let show_error_with_log = Pools.with_log

let log_status database =
  let tags = database |> label |> Logger.Tags.create in
  function
  | Ok _ ->
    Logs.info (fun m -> m ~tags "Database connected: %s" ([%show: t] database))
  | Error conn ->
    Logs.info (fun m ->
      m
        ~tags
        "Database connection failed: [%s] [%s]"
        ([%show: t] database)
        (Caqti_error.show conn))
;;

module Root = struct
  let label = root

  let add () =
    let database = database_url () |> create label in
    let status = add_pool ~pool_size:(pool_size ()) database in
    log_status database status;
    status
  ;;

  let setup = add %> Lwt.return

  let start () =
    let tags = Logger.Tags.create label in
    Logs.info (fun m -> m ~tags "Start database %a" Label.pp label);
    let%lwt (_ : status) = setup () in
    Lwt.return_unit
  ;;

  let stop () = Lwt.return_unit
end

module Tenant = struct
  let setup_tenant database =
    let () =
      add_pool ~pool_size:(pool_size ()) database |> log_status database
    in
    database |> Entity.label |> Lwt.return
  ;;

  let setup () =
    match%lwt Repo.find_all_running root with
    | [] -> failwith Pool_message.Error.(NoTenantsRegistered |> show)
    | tenants -> Lwt_list.map_s setup_tenant tenants
  ;;

  let find = Repo.find root

  let find_all_running () =
    Repo.find_all_running root |> Lwt.map (CCList.map Entity.label)
  ;;

  let start () =
    let%lwt (_ : Label.t list) = setup () in
    Lwt.return_unit
  ;;

  let stop () = Lwt.return_unit
end

let test_and_create url label =
  let%lwt connection =
    let uri = url |> Url.value |> Uri.of_string in
    Caqti_lwt_unix.with_connection uri (fun (_ : Caqti_lwt.connection) ->
      Lwt_result.return ())
  in
  match connection with
  | Ok () -> create label url |> Lwt.return_ok
  | Error (_ : Caqti_error.load_or_connect) ->
    Lwt_result.fail Pool_message.(Error.Invalid Field.DatabaseUrl)
;;
