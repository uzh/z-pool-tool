open Caqti_request.Infix
module Dynparam = Database.Dynparam

(* MariaDB expects uuid to be bytes, since we can't unhex when using caqti's
   populate, we have to do that manually. *)
let to_bytes encode decode id =
  match id |> decode |> Uuidm.of_string with
  | Some uuid -> Uuidm.to_bytes uuid |> encode
  | None -> failwith "Invalid uuid provided"
;;

let sql_table = function
  | `Current -> "pool_queue_jobs"
  | `History -> "pool_queue_jobs_history"
;;

let tablename history = sql_table (if history then `History else `Current)

let sql_select_columns table =
  let go field =
    table
    |> CCOption.map_or ~default:field (fun tablename ->
      CCString.concat "." [ tablename; field ])
  in
  [ Entity.Id.sql_select_fragment ~field:(go "uuid")
  ; go "name"
  ; go "input"
  ; go "message_template"
  ; go "tries"
  ; go "max_tries"
  ; go "run_at"
  ; go "status"
  ; go "persisted_at"
  ; go "polled_at"
  ; go "handled_at"
  ; go "last_error"
  ; go "last_error_at"
  ; go "database_label"
  ; Entity.Id.sql_select_fragment ~field:(go "clone_of")
  ]
;;

let update_sql ?(where_fragment = "") table =
  [%string
    {sql|
      UPDATE %{table}
      SET
        name = $2,
        input = $3,
        message_template = $4,
        tries = $5,
        max_tries = $6,
        run_at = $7,
        status = $8,
        persisted_at = $9,
        polled_at = $10,
        handled_at = $11,
        last_error = $12,
        last_error_at = $13,
        database_label = $14,
        clone_of = $15
      %{where_fragment}
    |sql}]
;;

let update_request table =
  table
  |> update_sql
       ~where_fragment:
         [%string
           {sql| WHERE %{table}.uuid = %{Entity.Id.sql_value_fragment "$1"} |sql}]
  |> Repo_entity.Instance.t ->. Caqti_type.unit
;;

let update ?(history = false) label =
  Database.exec label (tablename history |> update_request)
;;

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count
    then "COUNT(*)"
    else sql_select_columns None |> CCString.concat ", "
  in
  [%string
    {sql|SELECT %{columns} FROM %{sql_table `Current}, %{sql_table `History} %{where_fragment}|sql}]
;;

let find_request =
  [%string {sql| WHERE uuid = %{Entity.Id.sql_value_fragment "?"} |sql}]
  |> Repo_entity.Id.t ->? Repo_entity.Instance.t
;;

let find label id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt label find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Queue)
;;

let find_by ?query pool =
  Query.collect_and_count
    pool
    query
    ~select:(fun ?count m -> find_request_sql ?count m)
    Repo_entity.Instance.t
;;

let find_workable_query ?(count = false) ?limit () =
  let columns =
    if count then "COUNT(*)" else sql_select_columns None |> CCString.concat ","
  in
  let limit = CCOption.map_or ~default:"" (Format.asprintf "LIMIT %i") limit in
  [%string
    {sql|
      SELECT %{columns} FROM %{sql_table `Current}
      WHERE status = "pending"
        AND name = ?
        AND polled_at IS NULL
        AND run_at <= NOW()
        AND tries < max_tries
      ORDER BY run_at ASC
      %{limit}
    |sql}]
;;

let find_workable_request =
  find_workable_query () |> Repo_entity.(JobName.t ->* Instance.t)
;;

let find_workable job label = Database.collect label find_workable_request job

let count_workable_request =
  find_workable_query ~count:true () |> Repo_entity.JobName.t ->? Caqti_type.int
;;

let count_workable job_name label =
  let open Utils.Lwt_result.Infix in
  Database.find_opt label count_workable_request job_name
  ||> CCOption.to_result Pool_message.Error.NoValue
;;

let poll_n_workable database_label n_instances job_name =
  let open Lwt_result.Syntax in
  let find_workable_request =
    find_workable_query ~limit:n_instances ()
    |> Repo_entity.(JobName.t ->* Instance.t)
  in
  Database.query database_label (fun connection ->
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    let* () = Connection.start () in
    Lwt.catch
      (fun () ->
        let* instances =
          Connection.collect_list find_workable_request job_name
        in
        let instances = CCList.map Entity.Instance.poll instances in
        Lwt_list.filter_map_s
          (fun instance ->
            let%lwt result =
              Connection.exec (update_request (sql_table `Current)) instance
            in
            if CCResult.is_ok result
            then Lwt.return_some instance
            else Lwt.return_none)
          instances
        |> Lwt_result.ok)
      (fun exn ->
        Logs.err (fun m -> m "Couldn't poll jobs: %s" (Printexc.to_string exn));
        let* () = Connection.rollback () in
        Lwt.return_ok []))
;;

let insert_request table =
  let table = sql_table table in
  [%string
    {sql|
      INSERT INTO %{table} (
        uuid,
        name,
        input,
        message_template,
        tries,
        max_tries,
        run_at,
        status,
        persisted_at,
        polled_at,
        handled_at,
        last_error,
        last_error_at,
        database_label,
        clone_of
      ) VALUES (
        %{Entity.Id.sql_value_fragment "$1"},
        $2,
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11,
        $12,
        $13,
        $14,
        %{Entity.Id.sql_value_fragment "$15"}
      )
    |sql}]
  |> Repo_entity.Instance.t ->. Caqti_type.unit
;;

let enqueue_request = insert_request `Current

let enqueue label job_instance =
  Database.exec label enqueue_request job_instance
;;

let populatable =
  let open Entity in
  let to_bytes = Id.(to_bytes of_string value) in
  CCList.map (fun ({ Instance.id; clone_of; _ } as job) ->
    { job with
      Instance.id = to_bytes id
    ; clone_of = CCOption.map to_bytes clone_of
    })
;;

let columns =
  [ "uuid"
  ; "name"
  ; "input"
  ; "message_template"
  ; "tries"
  ; "max_tries"
  ; "run_at"
  ; "status"
  ; "persisted_at"
  ; "polled_at"
  ; "handled_at"
  ; "last_error"
  ; "last_error_at"
  ; "database_label"
  ; "clone_of"
  ]
;;

let enqueue_all label = function
  | [] -> Lwt.return_unit
  | instances ->
    (* Lwt_list.iter_s (enqueue label) instances *)
    Database.query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.populate
        ~table:(sql_table `Current)
        ~columns
        Repo_entity.Instance.t
        (instances |> populatable |> CCList.rev |> Caqti_lwt.Stream.of_list)
      |> Lwt.map Caqti_error.uncongested)
;;

let delete_request =
  [%string
    {sql|
      DELETE FROM %{sql_table `Current}
      WHERE uuid = %{Entity.Id.sql_value_fragment "uuid"}
    |sql}]
  |> Repo_entity.Id.t ->. Caqti_type.unit
;;

let delete label (job : Entity.Instance.t) =
  Database.exec label delete_request job.Entity.Instance.id
;;

let archive_insert_request =
  let columns = columns |> CCString.concat "," in
  [%string
    {sql|
      INSERT INTO %{sql_table `History} (%{columns})
      SELECT %{columns} FROM %{sql_table `Current}
      WHERE uuid = %{Entity.Id.sql_value_fragment "?"}
    |sql}]
  |> Repo_entity.Id.t ->. Caqti_type.unit
;;

let archive { Entity.Instance.id; database_label; _ } =
  let open Lwt_result.Syntax in
  Database.query database_label (fun connection ->
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    let* () = Connection.start () in
    Lwt.catch
      (fun () ->
        let* () = Connection.exec archive_insert_request id in
        Connection.exec delete_request id)
      (fun exn ->
        Logs.err (fun m -> m "Job archive failed: %s" (Printexc.to_string exn));
        Connection.rollback ()))
;;

let clean_request = "TRUNCATE TABLE queue_jobs" |> Caqti_type.(unit ->. unit)
let clean label () = Database.exec label clean_request ()
let filter_fragment = {sql| WHERE tag LIKE $1 |sql}

let search_query =
  [%string
    {sql|
      SELECT
        COUNT(*) OVER() as total,
        %{sql_select_columns None |> CCString.concat ", "}
      FROM %{sql_table `Current}, %{sql_table `History}
    |sql}]
;;

let register_cleaner () =
  Sihl.Cleaner.register_cleaner (fun ?ctx () ->
    clean
      CCOption.(
        map Database.of_ctx_exn ctx
        |> get_exn_or Pool_message.(Error.(NotFound Field.Context |> show)))
      ())
;;

module MakeColumns (Table : sig
    val name : string
  end) =
struct
  open Pool_message.Field
  open Query.Column
  open Query.Filter

  let column_job_name = (Name, [%string "%{Table.name}.name"]) |> create
  let column_job_status = (Status, [%string "%{Table.name}.status"]) |> create
  let column_error = (LastError, [%string "%{Table.name}.last_error"]) |> create

  let column_error_at =
    (LastErrorAt, [%string "%{Table.name}.last_error_at"]) |> create
  ;;

  let column_run_at = (NextRunAt, [%string "%{Table.name}.run_at"]) |> create
  let column_input = (Input, [%string "%{Table.name}.input"]) |> create

  open Entity
  open JobName

  let job_name_filter =
    let options = build_options all show in
    Condition.Human.Select (column_job_name, options)
  ;;

  let job_status_filter =
    let options = build_options all show in
    Condition.Human.Select (column_job_status, options)
  ;;

  let searchable_by = [ column_input ]

  let sortable_by =
    [ column_job_name
    ; column_job_status
    ; column_error
    ; column_error_at
    ; column_run_at
    ]
  ;;

  let filterable_by = Some [ job_name_filter; job_status_filter ]

  let default_sort =
    Query.Sort.{ column = column_run_at; order = SortOrder.Descending }
  ;;

  let default_query = Query.create ~sort:default_sort ()
end

module Jobs = MakeColumns (struct
    let name = sql_table `Current
  end)

module JobHistory = MakeColumns (struct
    let name = sql_table `History
  end)
