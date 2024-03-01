open CCFun.Infix

let src = Logs.Src.create "database"
let flat_unit (_ : unit list) = ()

module Logger = struct
  module Tags = struct
    let add_label : string Logs.Tag.def =
      Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
    ;;

    let create database = Logs.Tag.(empty |> add add_label database)
  end
end

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let prefix t x (Pack (t', x')) = Pack (Caqti_type.t2 t t', (x, x'))
  let add t x (Pack (t', x')) = Pack (Caqti_type.t2 t' t, (x', x))
end

let raise_caqti_error ?tags =
  let open Caqti_error in
  function
  | Error `Unsupported ->
    Logs.err ~src (fun m -> m ?tags "Caqti error unsupported");
    failwith "Caqti error unsupported"
  | (Error #t | Ok _) as x ->
    (match x with
     | Ok res -> res
     | Error err ->
       Logs.err ~src (fun m -> m ?tags "%s" @@ show err);
       failwith (show err))
;;

module Make (Database : Database_pools_sig.Sig) = struct
  let to_ctx label = [ "pool", label ]
  let collect label = Database.collect ~ctx:(to_ctx label)
  let exec label = Database.exec ~ctx:(to_ctx label)
  let find label = Database.find ~ctx:(to_ctx label)
  let find_opt label = Database.find_opt ~ctx:(to_ctx label)
  let transaction label = Database.transaction ~ctx:(to_ctx label)
  let transaction_exn label = Database.transaction_exn ~ctx:(to_ctx label)

  let find_as_transaction database_label ?(setup = []) ?(cleanup = []) fnc =
    let open Lwt_result.Syntax in
    let fnc connection =
      let exec_each =
        Lwt_list.map_s (fun request -> request connection)
        %> Lwt.map CCResult.flatten_l
        %> Lwt_result.map flat_unit
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
      let exec_each = Lwt_list.iter_s (fun request -> request connection) in
      exec_each commands |> Lwt_result.return
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
            ( dyn |> Dynparam.add Caqti_type.string (id |> decode_id)
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
    Database.query
      ~ctx:[ "pool", database_label ]
      (fun connection ->
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
    let open Lwt_result.Syntax in
    let tags = Logger.Tags.create database_label in
    let truncate_table table =
      Logs.debug ~src (fun m -> m ~tags "Truncate table '%s'" table);
      CCFormat.asprintf "TRUNCATE TABLE %s" table |> Caqti_type.(unit ->. unit)
    in
    let* truncate_reqs =
      ()
      |> collect database_label truncate_table_names_request
      |> Lwt_result.map (CCList.map truncate_table)
    in
    let manual_cleanups = [ message_templates_cleanup_requeset ] in
    Lwt.return_ok (truncate_reqs @ manual_cleanups)
  ;;

  let clean_all database_label =
    let open Lwt_result.Syntax in
    let* clean_reqs = clean_requests database_label in
    with_disabled_fk_check database_label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s (fun request -> Connection.exec request ()) clean_reqs
      |> Lwt.map CCResult.flatten_l
      |> Lwt_result.map flat_unit)
  ;;
end
