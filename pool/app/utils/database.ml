module Lwt_result = Lwt_trace

let with_transaction _ f =
  (* TODO with_transaction *)
  f ()
;;

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
end

let raise_caqti_error =
  let open Caqti_error in
  function
  | Error `Unsupported ->
    Logs.err (fun m -> m "Caqti error unsupported");
    failwith "Caqti error unsupported"
  | (Error #t | Ok _) as x ->
    (match x with
     | Ok res -> res
     | Error err ->
       Logs.err (fun m -> m "%s" @@ show err);
       failwith (show err))
;;

let find db_pool request input =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", db_pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input ||> raise_caqti_error)
;;

let find_opt db_pool request input =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", db_pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input ||> raise_caqti_error)
;;

let collect db_pool request input =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", db_pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input ||> raise_caqti_error)
;;

let exec db_pool request input =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", db_pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input ||> raise_caqti_error)
;;

let transaction pool commands =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      ||> CCResult.flatten_l
      >|+ ignore
      ||> raise_caqti_error)
;;

let transaction_find_opt pool commands query =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      ||> CCResult.flatten_l
      |> CCFun.const @@ Connection.collect_list query input
      ||> raise_caqti_error)
;;

let transaction_collect pool commands query =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      ||> CCResult.flatten_l
      |> CCFun.const @@ Connection.collect_list query input
      ||> raise_caqti_error)
;;

let set_fk_check_request =
  let open Caqti_request.Infix in
  "SET FOREIGN_KEY_CHECKS = ?" |> Caqti_type.(bool ->. unit)
;;

let with_disabled_fk_check db_pool f =
  let open Lwt_result.Infix in
  Sihl.Database.query
    ~ctx:[ "pool", db_pool ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      let%lwt () =
        Connection.exec set_fk_check_request false ||> raise_caqti_error
      in
      (f connection)
        (* Use PPX for backtrace *)
        [%lwt.finally
          Connection.exec set_fk_check_request true ||> raise_caqti_error])
;;

(** [table_names_request] request to return all table names

    Skipped database tables:

    - core_migration_state: migration state of the application
    - email_templates: clean up is handled by RestoreDefault mail event *)
let table_names_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT TABLE_NAME
    FROM INFORMATION_SCHEMA.`TABLES`
    WHERE TABLE_SCHEMA IN (DATABASE()) AND TABLE_NAME NOT IN ('core_migration_state', 'email_templates')
  |sql}
  |> Caqti_type.(unit ->* string) ~oneshot:true
;;

let clean_requests db_pool =
  let open Caqti_request.Infix in
  let open Lwt_result.Infix in
  let truncate_table table =
    Logs.debug (fun m -> m "Truncate table '%s' from pool '%s'" table db_pool);
    CCFormat.asprintf "TRUNCATE TABLE %s" table |> Caqti_type.(unit ->. unit)
  in
  () |> collect db_pool table_names_request ||> CCList.map truncate_table
;;

let clean_all db_pool =
  let%lwt clean_reqs = clean_requests db_pool in
  with_disabled_fk_check db_pool (fun connection ->
    let open Lwt_result.Infix in
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    Lwt_list.iter_s
      (fun request -> Connection.exec request () ||> raise_caqti_error)
      clean_reqs)
;;
