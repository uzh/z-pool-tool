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
  Sihl.Database.query ~ctx:[ "pool", db_pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input |> Lwt.map raise_caqti_error)
;;

let find_opt db_pool request input =
  Sihl.Database.query ~ctx:[ "pool", db_pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input |> Lwt.map raise_caqti_error)
;;

let collect db_pool request input =
  Sihl.Database.query ~ctx:[ "pool", db_pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input |> Lwt.map raise_caqti_error)
;;

let exec db_pool request input =
  Sihl.Database.query ~ctx:[ "pool", db_pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input |> Lwt.map raise_caqti_error)
;;

let transaction pool commands =
  Sihl.Database.query ~ctx:[ "pool", pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      |> Lwt.map CCResult.flatten_l
      |> Lwt_result.map ignore
      |> Lwt.map raise_caqti_error)
;;

let transaction_find_opt pool commands query =
  let ( >> ) = CCFun.const in
  Sihl.Database.query ~ctx:[ "pool", pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      |> Lwt.map CCResult.flatten_l
      >> Connection.find_opt query input
      |> Lwt.map raise_caqti_error)
;;

let transaction_collect pool commands query =
  let ( >> ) = CCFun.const in
  Sihl.Database.query ~ctx:[ "pool", pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      |> Lwt.map CCResult.flatten_l
      >> Connection.collect_list query input
      |> Lwt.map raise_caqti_error)
;;

let set_fk_check_request =
  Caqti_request.exec Caqti_type.bool "SET FOREIGN_KEY_CHECKS = ?;"
;;

let with_disabled_fk_check db_pool f =
  let open Lwt.Syntax in
  Sihl.Database.query ~ctx:[ "pool", db_pool ] (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      let* () =
        Connection.exec set_fk_check_request false |> Lwt.map raise_caqti_error
      in
      Lwt.finalize
        (fun () -> f connection)
        (fun () ->
          Connection.exec set_fk_check_request true |> Lwt.map raise_caqti_error))
;;

let table_names_request =
  Caqti_request.collect
    ~oneshot:true
    Caqti_type.unit
    Caqti_type.string
    {sql|
      SELECT TABLE_NAME
      FROM INFORMATION_SCHEMA.`TABLES`
      WHERE TABLE_SCHEMA IN (DATABASE()) AND TABLE_NAME NOT IN ('core_migration_state','email_templates');
    |sql}
;;

let clean_requests db_pool =
  let open Lwt.Infix in
  let truncate_table table =
    Logs.debug (fun m -> m "Truncate table '%s' from pool '%s'" table db_pool);
    Caqti_request.exec
      Caqti_type.unit
      (Format.asprintf "TRUNCATE TABLE %s" table)
  in
  () |> collect db_pool table_names_request >|= List.map truncate_table
;;

let clean_all db_pool =
  let open Lwt.Infix in
  clean_requests db_pool
  >>= fun clean_requests ->
  with_disabled_fk_check db_pool (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.iter_s
        (fun request -> Connection.exec request () |> Lwt.map raise_caqti_error)
        clean_requests)
;;
