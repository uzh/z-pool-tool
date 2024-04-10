open CCFun.Infix
module Guard = Guardian_backend.Pools.Make (Entity.MariaConfig)
module Core = Pools.Make (Entity.MariaConfigPool)

type status = Core.status

let src = Logs.Src.create "database"

module Logs = (val Logs.src_log src : Logs.LOG)

let raise_caqti_error ?tags =
  let open Caqti_error in
  function
  | Ok resp -> resp
  | Error `Unsupported ->
    Logs.err (fun m -> m ?tags "Caqti error unsupported");
    failwith "Caqti error unsupported"
  | Error (#t as err) -> raise (Exn err)
;;

let to_ctx = Entity.to_ctx
let create_tag label = Logger.Tags.create label

let add_pool ?required ?pool_size database =
  Core.add_pool
    ?required
    ?pool_size
    (database |> Entity.label |> Entity.Label.value)
    (database |> Entity.url |> Entity.Url.value)
;;

let drop_pool = Entity.Label.value %> Core.drop_pool
let fetch_pool label = Core.fetch_pool ~ctx:(to_ctx label)
let query label = Core.query ~ctx:(to_ctx label)

let collect label request =
  Core.collect ~ctx:(to_ctx label) request
  %> Lwt.map (raise_caqti_error ~tags:(create_tag label))
;;

let exec label request =
  Core.exec ~ctx:(to_ctx label) request
  %> Lwt.map (raise_caqti_error ~tags:(create_tag label))
;;

let find label request =
  Core.find ~ctx:(to_ctx label) request
  %> Lwt.map (raise_caqti_error ~tags:(create_tag label))
;;

let find_opt label request =
  Core.find_opt ~ctx:(to_ctx label) request
  %> Lwt.map (raise_caqti_error ~tags:(create_tag label))
;;

let populate label table columns request =
  Core.populate ~ctx:(to_ctx label) table columns request
  %> Lwt.map (raise_caqti_error ~tags:(create_tag label))
;;

let transaction label =
  Core.transaction ~ctx:(to_ctx label)
  %> Lwt.map (raise_caqti_error ~tags:(create_tag label))
;;

let transaction_exn label = Core.transaction_exn ~ctx:(to_ctx label)

let find_as_transaction database_label ?(setup = []) ?(cleanup = []) fnc =
  let open Lwt_result.Syntax in
  let fnc connection =
    let exec_each =
      Lwt_list.map_s (fun request -> request connection)
      %> Lwt.map CCResult.flatten_l
      %> Lwt_result.map Utils.flat_unit
    in
    let* () = exec_each setup in
    let* result = fnc connection in
    let* () = exec_each cleanup in
    Lwt.return @@ Ok result
  in
  transaction database_label fnc
;;

let exec_as_transaction database_label commands =
  let fnc connection =
    Lwt_list.map_s (fun request -> request connection) commands
    |> Lwt.map CCResult.flatten_l
    |> Lwt_result.map Utils.flat_unit
  in
  transaction database_label fnc
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
  Core.query ~ctx:(to_ctx database_label) (fun connection ->
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

let clean_all_exn database_label =
  let open Utils.Lwt_result.Infix in
  clean_all database_label
  ||> Pools.get_or_raise ~ctx:(to_ctx database_label) ()
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
