let src = Logs.Src.create "database"

module Logs = (val Logs.src_log src : Logs.LOG)

module MariaConfigPool = struct
  let database = Entity.(create root (database_url ()))
  let database_pool_size = 10
  let expected_databases = 5
end

module Guard = Pools_guardian.Make (MariaConfigPool)
include Pools.Make (MariaConfigPool)

let exec_query request input (module Connection : Caqti_lwt.CONNECTION) =
  Connection.exec request input
;;

let exclude_ids column_name decode_id dyn exclude =
  let sql = "UNHEX(REPLACE(?, '-', ''))" in
  match exclude with
  | [] -> dyn, None
  | exclude ->
    let dyn, sql_strings =
      CCList.fold_left
        (fun (dyn, sql_strings) id ->
           ( dyn |> Entity.Dynparam.add Caqti_type.string (id |> decode_id)
           , sql :: sql_strings ))
        (dyn, [])
        exclude
    in
    sql_strings
    |> CCString.concat ", "
    |> Format.asprintf "%s NOT IN (%s)" column_name
    |> CCOption.return
    |> CCPair.make dyn
;;

let set_fk_check_request =
  let open Caqti_request.Infix in
  "SET FOREIGN_KEY_CHECKS = ?" |> Caqti_type.(bool ->. unit)
;;

let with_disabled_fk_check db_ctx f =
  let open Lwt_result.Syntax in
  query db_ctx (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    let* () = Connection.exec set_fk_check_request false in
    (f connection)
      (* Use PPX for backtrace *)
      [%lwt.finally
        Connection.exec set_fk_check_request true
        |> Lwt_result.map_error Caqti_error.show
        |> Lwt.map CCResult.get_or_failwith])
;;

let message_templates_cleanup_requeset =
  let open Caqti_request.Infix in
  {sql|
      DELETE FROM pool_message_templates
      WHERE entity_uuid IS NOT NULL
    |sql}
  |> Caqti_type.(unit ->. unit)
;;

(** [truncate_table_names_request] request to return all table names

    Skipped database tables:
    - core_migration_state: migration state of the application
    - pool_message_templates
    - guardian_role_permissions
    - pool_i18n
    - pool_system_settings *)

let truncate_table_names_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT TABLE_NAME
      FROM INFORMATION_SCHEMA.`TABLES`
      WHERE TABLE_SCHEMA IN (DATABASE()) AND TABLE_NAME NOT IN ('core_migration_state', 'pool_message_templates', 'pool_i18n', 'pool_system_settings')
    |sql}
  |> Caqti_type.(unit ->* string) ~oneshot:true
;;

let clean_requests db_ctx =
  let open Caqti_request.Infix in
  let tags = Logger.Tags.of_db_ctx db_ctx in
  let truncate_table table =
    Logs.debug (fun m -> m ~tags "Truncate table '%s'" table);
    CCFormat.asprintf "TRUNCATE TABLE %s" table |> Caqti_type.(unit ->. unit)
  in
  let%lwt truncate_reqs =
    ()
    |> collect db_ctx truncate_table_names_request
    |> Lwt.map (CCList.map truncate_table)
  in
  let manual_cleanups = [ message_templates_cleanup_requeset ] in
  Lwt.return (truncate_reqs @ manual_cleanups)
;;

let clean_root_requests () =
  let open Caqti_request.Infix in
  let tags = Logger.Tags.create Entity.root in
  let truncate_table table =
    Logs.debug (fun m -> m ~tags "Truncate root table '%s'" table);
    CCFormat.asprintf "TRUNCATE TABLE %s" table |> Caqti_type.(unit ->. unit)
  in
  let tables =
    [ "pool_announcements"; "pool_announcement_users_hide"; "pool_announcement_tenants" ]
  in
  CCList.map truncate_table tables
;;

let clean_all db_ctx =
  let%lwt clean_reqs = clean_requests db_ctx in
  let clean_root_reqs = clean_root_requests () in
  let exec_clean_req (requests, db_ctx) =
    with_disabled_fk_check db_ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s (fun request -> Connection.exec request ()) requests
      |> Lwt.map CCResult.flatten_l
      |> Lwt_result.map Utils.flat_unit)
  in
  let%lwt () = exec_clean_req (clean_reqs, db_ctx) in
  let root = label_ctx Entity.root in
  exec_clean_req (clean_root_reqs, root)
;;

type any_ctx = Any : 'maybe_txn Entity.ctx -> any_ctx [@@ocaml.unboxed]

let resolve_ctx (type maybe_txn) ?(db_ctx : maybe_txn Entity.ctx option) label =
  match db_ctx with
  | None -> Any (label_ctx label)
  | Some db_ctx ->
    assert (String.equal (Entity.label_of_ctx db_ctx) label);
    Any db_ctx
