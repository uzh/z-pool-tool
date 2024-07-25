open Caqti_request.Infix
open Utils.Lwt_result.Infix
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

let sql_select_columns ?(decode = true) table =
  let go field =
    table
    |> CCOption.map_or ~default:field (fun tablename ->
      CCString.concat "." [ tablename; field ])
  in
  let go_binary field =
    if decode then Entity.Id.sql_select_fragment ~field:(go field) else field
  in
  [ go_binary "uuid"
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
  ; go_binary "clone_of"
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

let find_request table =
  let columns =
    Some (sql_table table) |> sql_select_columns |> CCString.concat ", "
  in
  [%string
    {sql|
      SELECT %{columns} FROM %{sql_table table}
      WHERE uuid = %{Entity.Id.sql_value_fragment "?"}
    |sql}]
  |> Repo_entity.Id.t ->? Repo_entity.Instance.t
;;

let find label id =
  let find_in table = Database.find_opt label (find_request table) id in
  find_in `History
  >|> (function
         | Some element -> Lwt.return_some element
         | None -> find_in `Current)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Queue)
;;

let find_combined_request_sql ?(count = false) where_fragment =
  let columns ?(count = false) ?decode () =
    if count
    then "COUNT(*)"
    else sql_select_columns ?decode None |> CCString.concat ", "
  in
  [%string
    {sql|
      SELECT %{columns ~count ()} FROM (
        SELECT %{columns ~decode:false ()} FROM %{sql_table `History}
        UNION ALL
        SELECT %{columns ~decode:false ()} FROM %{sql_table `Current}
        ) queue
      %{where_fragment}
    |sql}]
;;

let find_by ?query pool =
  Query.collect_and_count
    pool
    query
    ~select:find_combined_request_sql
    Repo_entity.Instance.t
;;

let workable_where =
  {sql|
    status = "pending"
    AND polled_at IS NULL
    AND run_at <= NOW()
    AND tries < max_tries
  |sql}
;;

let find_workable_query ?(count = false) ?limit () =
  let columns =
    if count then "COUNT(*)" else sql_select_columns None |> CCString.concat ","
  in
  let limit = CCOption.map_or ~default:"" (Format.asprintf "LIMIT %i") limit in
  [%string
    {sql|
      SELECT %{columns} FROM %{sql_table `Current}
      WHERE name = ? AND %{workable_where}
      ORDER BY run_at ASC
      %{limit}
    |sql}]
;;

let find_workable_request =
  find_workable_query () |> Repo_entity.(JobName.t ->* Instance.t)
;;

let find_workable job label = Database.collect label find_workable_request job

let count_all_workable_request =
  [%string
    {sql|
      SELECT COUNT(*) FROM %{sql_table `Current}
      WHERE %{workable_where}
    |sql}]
  |> Caqti_type.(unit ->! int)
;;

let count_all_workable label =
  Database.find_opt label count_all_workable_request ()
  ||> CCOption.get_or ~default:0
  ||> CCResult.return
;;

let count_workable_request =
  find_workable_query ~count:true () |> Repo_entity.JobName.t ->? Caqti_type.int
;;

let count_workable job_name label =
  Database.find_opt label count_workable_request job_name
  ||> CCOption.to_result Pool_message.Error.NoValue
;;

let poll_n_workable database_label n_instances job_name =
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
        let%lwt instances =
          CCList.map Entity.Instance.poll instances
          |> Lwt_list.filter_map_s (fun instance ->
            Connection.exec (update_request (sql_table `Current)) instance
            ||> function
            | Ok () -> Some instance
            | Error _ -> None)
        in
        let* () = Connection.commit () in
        Lwt.return_ok instances)
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
let enqueue label = Database.exec label enqueue_request

let populatable =
  let open Entity in
  let to_bytes = Id.(to_bytes of_string value) in
  CCList.map (fun ({ Instance.id; clone_of; _ } as job) ->
    { job with
      Instance.id = to_bytes id
    ; clone_of = CCOption.map to_bytes clone_of
    })
;;

let enqueue_all label = function
  | [] -> Lwt.return_unit
  | instances ->
    let table = sql_table `Current in
    let columns = sql_select_columns ~decode:false (Some table) in
    Database.query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.populate
        ~table
        ~columns
        Repo_entity.Instance.t
        (instances |> populatable |> CCList.rev |> Caqti_lwt.Stream.of_list)
      ||> Caqti_error.uncongested)
;;

let reset_pending_request =
  [%string
    {sql|
      UPDATE %{sql_table `Current}
      SET
        polled_at = NULL,
        handled_at = NULL
      WHERE status = 'pending'
        AND (polled_at IS NOT NULL OR handled_at IS NOT NULL)
    |sql}]
  |> Caqti_type.(unit ->. unit)
;;

let reset_pending_jobs label = Database.exec label reset_pending_request ()

let delete_request =
  [%string
    {sql|
      DELETE FROM %{sql_table `Current}
      WHERE uuid = %{Entity.Id.sql_value_fragment "?"}
    |sql}]
  |> Repo_entity.Id.t ->. Caqti_type.unit
;;

let archive_insert_request =
  let columns = sql_select_columns ~decode:false None |> CCString.concat "," in
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
        let* () = Connection.exec delete_request id in
        Connection.commit ())
      (fun exn ->
        Logs.err (fun m -> m "Job archive failed: %s" (Printexc.to_string exn));
        Connection.rollback ()))
;;

let find_archivable_request =
  [%string
    {sql|
      SELECT %{Entity.Id.sql_select_fragment ~field:("uuid")}
      FROM %{sql_table `Current}
      WHERE status != "pending"
    |sql}]
  |> Caqti_type.unit ->* Repo_entity.Id.t
;;

let archive_all_processed database_label =
  Database.query database_label (fun connection ->
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    let* ids = Connection.collect_list find_archivable_request () in
    let* () = Connection.start () in
    Lwt.catch
      (fun () ->
        let* () =
          Lwt_list.map_s
            (fun id ->
              let* () = Connection.exec archive_insert_request id in
              Connection.exec delete_request id)
            ids
          ||> CCResult.flatten_l
          >|+ Utils.flat_unit
        in
        Connection.commit ())
      (fun exn ->
        Logs.err (fun m -> m "Job archive failed: %s" (Printexc.to_string exn));
        Connection.rollback ()))
;;

module type ColumnsSig = sig
  val column_job_name : Query.Column.t
  val column_job_status : Query.Column.t
  val column_error : Query.Column.t
  val column_error_at : Query.Column.t
  val column_run_at : Query.Column.t
  val column_input : Query.Column.t
  val job_name_filter : Query.Filter.Condition.Human.t
  val job_status_filter : Query.Filter.Condition.Human.t
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list
  val filterable_by : Query.Filter.Condition.Human.t list option
  val default_sort : Query.Sort.t
  val default_query : Query.t
end

module MakeColumns (Table : sig
    val name : string option
  end) : ColumnsSig = struct
  open CCFun.Infix
  open Pool_message.Field
  open Query.Column
  open Query.Filter

  let concat = CCList.cons' (CCOption.to_list Table.name) %> CCString.concat "."
  let column_job_name = (Name, concat "name") |> create
  let column_job_status = (Status, concat "status") |> create
  let column_error = (LastError, concat "last_error") |> create
  let column_error_at = (LastErrorAt, concat "last_error_at") |> create
  let column_run_at = (NextRunAt, concat "run_at") |> create
  let column_input = (Input, concat "input") |> create

  open Entity
  open JobName

  let job_name_filter =
    let options = build_options all show in
    Condition.Human.Select (column_job_name, options)
  ;;

  let job_status_filter =
    let open Status in
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
