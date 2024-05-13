let src = Logs.Src.create "database"

module Logs = (val Logs.src_log src : Logs.LOG)

module MariaConfigPool = struct
  let database = Entity.(create root (database_url ()))
  let database_pool_size = 10
  let expected_databases = 5
end

module MariaConfig = struct
  open Guardian_backend.Pools
  include DefaultConfig

  let database =
    MultiPools Entity.[ root |> Label.value, database_url () |> Url.value ]
  ;;
end

module Guard = Guardian_backend.Pools.Make (MariaConfig)
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

let with_disabled_fk_check database_label f =
  let open Lwt_result.Syntax in
  query database_label (fun connection ->
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

let clean_requests database_label =
  let open Caqti_request.Infix in
  let tags = Logger.Tags.create database_label in
  let truncate_table table =
    Logs.debug (fun m -> m ~tags "Truncate table '%s'" table);
    CCFormat.asprintf "TRUNCATE TABLE %s" table |> Caqti_type.(unit ->. unit)
  in
  let%lwt truncate_reqs =
    ()
    |> collect database_label truncate_table_names_request
    |> Lwt.map (CCList.map truncate_table)
  in
  let manual_cleanups = [ message_templates_cleanup_requeset ] in
  Lwt.return (truncate_reqs @ manual_cleanups)
;;

let clean_all database_label =
  let%lwt clean_reqs = clean_requests database_label in
  with_disabled_fk_check database_label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    Lwt_list.map_s (fun request -> Connection.exec request ()) clean_reqs
    |> Lwt.map CCResult.flatten_l
    |> Lwt_result.map Utils.flat_unit)
;;

let add_pool ?required ?(pool_size = 10) (model : Entity.t) =
  let status = add_pool ?required ~pool_size model in
  let () =
    Guard.add_pool
      ~pool_size
      Entity.(label model |> Label.value)
      Entity.(url model |> Url.value)
  in
  status
;;

let drop_pool label =
  let%lwt () = drop_pool label in
  let%lwt () = Guard.drop_pool (Entity.Label.value label) in
  Lwt.return_unit
;;
