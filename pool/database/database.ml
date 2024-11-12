open CCFun
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

module Pool = struct
  include Service.Pool

  let all ?allowed_status ?exclude =
    find_all ?allowed_status ?exclude %> CCList.map Entity.label
  ;;

  let add_guard_pool { Entity.label; url; _ } =
    Guard.add_pool (Entity.Label.value label) (Entity.Url.value url)
  ;;

  let add ?required (model : Entity.t) =
    let status = add ?required model in
    let () = add_guard_pool model in
    status
  ;;

  let drop label =
    let%lwt () = drop label in
    let%lwt () = Guard.drop_pool (Entity.Label.value label) in
    Lwt.return_unit
  ;;

  let connect = connect %> Lwt.return

  let create_tested label url =
    match%lwt test_connection url with
    | Ok () -> create label url |> Lwt.return_ok
    | Error (_ : Caqti_error.load_or_connect) ->
      Lwt_result.fail Pool_message.(Error.Invalid Field.DatabaseUrl)
  ;;

  let create_validated_and_tested database_label database_url =
    let open Utils.Lwt_result.Infix in
    let* label = Label.create database_label |> Lwt_result.lift in
    let* url = Url.create database_url |> Lwt_result.lift in
    create_tested url label
  ;;

  let is_root = is_root

  module Root = struct
    let label = root
    let add = database_url %> create label %> add
    let setup = add %> Lwt.return

    let start () =
      log_start label;
      setup ()
    ;;

    let stop () = Lwt.return_unit
  end

  module Tenant = struct
    let drop = drop
    let find = find

    let add database =
      let () = add database in
      label database
    ;;

    let setup () =
      let start_tenant = tap (label %> log_start) %> add in
      match%lwt Repo.find_all_by_status root with
      | [] ->
        Logs.warn (fun m ->
          m "%s" Pool_message.Error.(NoTenantsRegistered |> show));
        Lwt.return []
      | tenants -> CCList.map start_tenant tenants |> Lwt.return
    ;;

    let all ?(status = Status.[ Active; ConnectionIssue; MigrationsPending ]) ()
      =
      find_by_status ~exclude:[ root ] status |> CCList.map Entity.label
    ;;

    let find_status_by_label = find %> CCOption.(of_result %> map Entity.status)

    let find_label_by_url ?allowed_status =
      find_by_url ?allowed_status %> CCResult.map Entity.label
    ;;

    let set_migration_pending = Repo.set_migration_pending

    let start () =
      let%lwt (_ : Label.t list) = setup () in
      Lwt.return_unit
    ;;

    let stop () = Lwt.return_unit

    let reset database_label =
      let open Utils.Lwt_result.Infix in
      Repo.find root database_label
      >|> function
      | Ok database ->
        let%lwt () = Pool.reset ~required:false database in
        let%lwt () = Guard.drop_pool (Entity.Label.value database_label) in
        add_guard_pool database |> Lwt.return
      | Error _ ->
        let%lwt () = drop database_label in
        Guard.drop_pool (Entity.Label.value database_label)
    ;;

    let update_status label status =
      let%lwt () = Repo.update_status root label status in
      reset label
    ;;

    let test_connection database_label =
      let open Utils.Lwt_result.Infix in
      let* database = Repo.find root database_label in
      match%lwt database |> url |> test_connection with
      | Ok () -> Lwt.return_ok ()
      | Error (_ : Caqti_error.load_or_connect) ->
        let%lwt () = update_status (database |> label) Status.ConnectionIssue in
        Lwt_result.fail Pool_message.(Error.SessionTenantNotFound)
    ;;
  end

  let initialize ?(clear = false) () =
    let () = if clear then Pool.clear () else () in
    let () = Root.add () in
    let%lwt (_ : Label.t list) = Tenant.setup () in
    Lwt.return_unit
  ;;
end
