module Lwt_result = Lwt_trace

let src = Logs.Src.create "utils.database"

module Logger = struct
  module Tags = struct
    let add_label : string Logs.Tag.def =
      Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
    ;;

    let create database = Logs.Tag.(empty |> add add_label database)
  end
end

let with_transaction _ f =
  (* TODO with_transaction *)
  f ()
;;

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
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

let find database_label request input =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input ||> raise_caqti_error ~tags)
;;

let find_opt database_label request input =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input ||> raise_caqti_error ~tags)
;;

let collect database_label request input =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input ||> raise_caqti_error ~tags)
;;

let exec database_label request input =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input ||> raise_caqti_error ~tags)
;;

let as_transaction database_label ~setup ~cleanup fnc =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  let pool = Sihl.Database.fetch_pool ~ctx:[ "pool", database_label ] () in
  Caqti_lwt.Pool.use
    (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let%lwt start_result = Connection.start () in
      match start_result with
      | Error err ->
        Logs.err ~src (fun m ->
          m ~tags "Failed to start transaction: %s" (Caqti_error.show err));
        Lwt.return @@ Error err
      | Ok () ->
        let exec_each =
          Lwt_list.iter_s (fun request ->
            request connection ||> raise_caqti_error ~tags)
        in
        Logs.debug ~src (fun m -> m ~tags "Started transaction");
        Lwt.catch
          (fun () ->
            let%lwt () = exec_each setup in
            let%lwt result = fnc connection in
            let%lwt () = exec_each cleanup in
            Lwt.return result)
          (fun exn ->
            (* Do we need commands to be passed to execute here? *)
            Connection.rollback ()
            >|> function
            | Ok () ->
              Logs.debug ~src (fun m ->
                m ~tags "Successfully rolled back transaction");
              Lwt.fail exn
            | Error err ->
              Logs.err ~src (fun m ->
                m
                  ~tags
                  "Failed to rollback transaction: %s"
                  (Caqti_error.show err));
              Lwt.fail exn))
    pool
  ||> raise_caqti_error ~tags
;;

let transaction database_label commands =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      ||> CCResult.flatten_l
      >|+ ignore
      ||> raise_caqti_error ~tags)
;;

let transaction_find_opt database_label commands query =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      ||> CCResult.flatten_l
      |> CCFun.const @@ Connection.collect_list query input
      ||> raise_caqti_error ~tags)
;;

let transaction_collect database_label commands query =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Lwt_list.map_s
        (fun (request, input) -> Connection.exec request input)
        commands
      ||> CCResult.flatten_l
      |> CCFun.const @@ Connection.collect_list query input
      ||> raise_caqti_error ~tags)
;;

let set_fk_check_request =
  let open Caqti_request.Infix in
  "SET FOREIGN_KEY_CHECKS = ?" |> Caqti_type.(bool ->. unit)
;;

let with_disabled_fk_check database_label f =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  Sihl.Database.query
    ~ctx:[ "pool", database_label ]
    (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      let%lwt () =
        Connection.exec set_fk_check_request false ||> raise_caqti_error ~tags
      in
      (f connection)
        (* Use PPX for backtrace *)
        [%lwt.finally
          Connection.exec set_fk_check_request true ||> raise_caqti_error ~tags])
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

let clean_requests database_label =
  let open Caqti_request.Infix in
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  let truncate_table table =
    Logs.debug ~src (fun m -> m ~tags "Truncate table '%s'" table);
    CCFormat.asprintf "TRUNCATE TABLE %s" table |> Caqti_type.(unit ->. unit)
  in
  () |> collect database_label table_names_request ||> CCList.map truncate_table
;;

let clean_all database_label =
  let open Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  let%lwt clean_reqs = clean_requests database_label in
  with_disabled_fk_check database_label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    Lwt_list.iter_s
      (fun request -> Connection.exec request () ||> raise_caqti_error ~tags)
      clean_reqs)
;;
