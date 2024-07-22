module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"queue_jobs.uuid"
  ; "queue_jobs.name"
  ; "queue_jobs.input"
  ; "queue_jobs.tries"
  ; "queue_jobs.next_run_at"
  ; "queue_jobs.max_tries"
  ; "queue_jobs.status"
  ; "queue_jobs.last_error"
  ; "queue_jobs.last_error_at"
  ; "queue_jobs.tag"
  ; "queue_jobs.ctx"
  ]
;;

let update_request =
  let open Caqti_request.Infix in
  [%string
    {sql|
      UPDATE queue_jobs
      SET
        name = $2,
        input = $3,
        tries = $4,
        next_run_at = $5,
        max_tries = $6,
        status = $7,
        last_error = $8,
        last_error_at = $9,
        tag = $10,
        ctx = $11
      WHERE
        queue_jobs.uuid = %{Pool_common.Id.sql_value_fragment "$1"}
    |sql}]
  |> Repo_entity.Instance.t ->. Caqti_type.unit
;;

let update label job_instance = Database.exec label update_request job_instance

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf {sql|SELECT %s FROM queue_jobs %s|sql} columns where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  [%string
    {sql|
      SELECT %{sql_select_columns |> CCString.concat ", "}
      FROM queue_jobs
      WHERE queue_jobs.uuid = %{Pool_common.Id.sql_value_fragment "?"}
    |sql}]
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
    ~select:find_request_sql
    Repo_entity.Instance.t
;;

let find_workable_query ?(count = false) () =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ","
  in
  Format.asprintf
    {sql|
      SELECT %s FROM queue_jobs
      WHERE status = "pending"
        AND next_run_at <= NOW()
        AND tries < max_tries
      ORDER BY id ASC
    |sql}
    columns
;;

let find_workable_request =
  let open Caqti_request.Infix in
  find_workable_query () |> Caqti_type.unit ->* Repo_entity.Instance.t
;;

let find_workable label = Database.collect label find_workable_request ()

let count_workable_request =
  let open Caqti_request.Infix in
  find_workable_query ~count:true () |> Caqti_type.(unit ->? int)
;;

let count_workable label =
  let open Utils.Lwt_result.Infix in
  Database.find_opt label count_workable_request ()
  ||> CCOption.to_result Pool_message.Error.NoValue
;;

let enqueue_request =
  let open Caqti_request.Infix in
  [%string
    {sql|
      INSERT INTO queue_jobs (
        uuid,
        name,
        input,
        tries,
        next_run_at,
        max_tries,
        status,
        last_error,
        last_error_at,
        tag,
        ctx
      ) VALUES (
        %{Pool_common.Id.sql_value_fragment "$1"},
        $2,
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9,
        $10,
        $11
      )
    |sql}]
  |> Repo_entity.Instance.t ->. Caqti_type.unit
;;

let enqueue label job_instance =
  Database.exec label enqueue_request job_instance
;;

(* MariaDB expects uuid to be bytes, since we can't unhex when using caqti's
   populate, we have to do that manually. *)
let populatable job_instances =
  let open Entity in
  job_instances
  |> CCList.map (fun ({ Instance.id; _ } as j) ->
    { j with
      Instance.id =
        (match id |> Entity.Id.value |> Uuidm.of_string with
         | Some uuid -> Uuidm.to_bytes uuid |> Entity.Id.of_string
         | None -> failwith "Invalid uuid provided")
    })
;;

let enqueue_all label job_instances =
  let () = Logs.info (fun m -> m "%s" "INSIDE ENQUEUE ALL") in
  let columns =
    [ "queue_jobs.uuid"
    ; "queue_jobs.name"
    ; "queue_jobs.input"
    ; "queue_jobs.tries"
    ; "queue_jobs.next_run_at"
    ; "queue_jobs.max_tries"
    ; "queue_jobs.status"
    ; "queue_jobs.last_error"
    ; "queue_jobs.last_error_at"
    ; "queue_jobs.tag"
    ; "queue_jobs.ctx"
    ]
  in
  job_instances
  |> populatable
  |> CCList.rev
  |> Database.populate label "queue_jobs" columns Repo_entity.Instance.t
;;

let query =
  let open Caqti_request.Infix in
  [%string
    {sql|
      SELECT %{sql_select_columns |> CCString.concat ", "}
      FROM queue_jobs
      ORDER BY next_run_at DESC
      LIMIT 100
    |sql}]
  |> Caqti_type.unit ->* Repo_entity.Instance.t
;;

let query label () = Database.collect label query ()

let delete_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM job_queues
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> Caqti_type.(string ->. unit)
;;

let delete label (job : Sihl.Contract.Queue.instance) =
  Database.exec label delete_request job.Sihl.Contract.Queue.id
;;

let clean_request =
  let open Caqti_request.Infix in
  "TRUNCATE TABLE queue_jobs" |> Caqti_type.(unit ->. unit)
;;

let clean label () = Database.exec label clean_request ()
let filter_fragment = {sql| WHERE queue_jobs.tag LIKE $1 |sql}

let search_query =
  [%string
    {sql|
      SELECT
        COUNT(*) OVER() as total,
        %{sql_select_columns |> CCString.concat ", "}
      FROM queue_jobs
    |sql}]
;;

module Migration = struct
  let fix_collation =
    Database.Migration.Step.create
      ~label:"fix collation"
      {sql|
        SET collation_server = 'utf8mb4_unicode_ci'
      |sql}
  ;;

  let create_jobs_table =
    Database.Migration.Step.create
      ~label:"create jobs table"
      {sql|
        CREATE TABLE IF NOT EXISTS queue_jobs (
          id BIGINT UNSIGNED AUTO_INCREMENT,
          uuid BINARY(16) NOT NULL,
          name VARCHAR(128) NOT NULL,
          input TEXT NULL,
          tries BIGINT UNSIGNED,
          next_run_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
          max_tries BIGINT UNSIGNED,
          status VARCHAR(128) NOT NULL,
          PRIMARY KEY (id),
          CONSTRAINT unique_uuid UNIQUE KEY (uuid)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}
  ;;

  let set_null_input_to_empty_string =
    Database.Migration.Step.create
      ~label:"set input to not null"
      {sql|
        UPDATE queue_jobs SET input = '' WHERE input IS NULL
      |sql}
  ;;

  let set_input_not_null =
    Database.Migration.Step.create
      ~label:"set input to not null"
      {sql|
        ALTER TABLE queue_jobs MODIFY COLUMN input TEXT NOT NULL DEFAULT ''
      |sql}
  ;;

  let add_error_columns =
    Database.Migration.Step.create
      ~label:"add error columns"
      {sql|
        ALTER TABLE queue_jobs
          ADD COLUMN last_error TEXT,
          ADD COLUMN last_error_at TIMESTAMP
      |sql}
  ;;

  let add_tag_column =
    Database.Migration.Step.create
      ~label:"add tag column"
      {sql|
        ALTER TABLE queue_jobs
          ADD COLUMN tag TEXT NULL
      |sql}
  ;;

  let add_ctx_column =
    Database.Migration.Step.create
      ~label:"add ctx column"
      {sql|
        ALTER TABLE queue_jobs
        ADD COLUMN ctx TEXT NULL
      |sql}
  ;;

  let migration =
    Database.Migration.(
      empty "queue"
      |> add_step fix_collation
      |> add_step create_jobs_table
      |> add_step set_null_input_to_empty_string
      |> add_step set_input_not_null
      |> add_step add_error_columns
      |> add_step add_tag_column
      |> add_step add_ctx_column)
  ;;
end

let register_cleaner () =
  Sihl.Cleaner.register_cleaner (fun ?ctx () ->
    clean
      CCOption.(
        map Database.of_ctx_exn ctx
        |> get_exn_or Pool_message.(Error.(NotFound Field.Context |> show)))
      ())
;;

let register_migration () =
  Database.Migration.register_migration Migration.migration
;;
