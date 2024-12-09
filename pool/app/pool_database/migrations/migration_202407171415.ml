let chunked name count_from query =
  [%string
    {sql|
      CREATE PROCEDURE %{name}(IN chunk_size INT)
      DETERMINISTIC
      BEGIN
          DECLARE offset_value INT DEFAULT 0;
          DECLARE total_rows INT DEFAULT 0;

          SELECT COUNT(*) INTO total_rows FROM %{count_from};

          WHILE offset_value < total_rows DO
              %{query}
              LIMIT chunk_size OFFSET offset_value
              ON DUPLICATE KEY UPDATE updated_at = NOW();

              SET offset_value = offset_value + chunk_size;
          END WHILE;
      END
    |sql}]
;;

let call ?(chunk_size = 10000) name =
  [%string {sql|CALL %{name}(%{CCInt.to_string chunk_size});|sql}]
;;

let drop name = [%string {sql|DROP PROCEDURE IF EXISTS %{name}|sql}]
let archive_jobs_name = "CopyRowsArchiveJobs"

let archive_queue_jobs_fcn =
  let count_from =
    {sql|
      (SELECT uuid FROM queue_jobs
      LEFT JOIN pool_message_history hist ON hist.queue_job_uuid = queue_jobs.uuid
      GROUP BY `uuid`) as queued_jobs
    |sql}
  in
  chunked
    archive_jobs_name
    count_from
    {sql|
      INSERT INTO pool_queue_jobs_history
        (uuid, name, input, message_template, tries, max_tries, run_at, status,
        persisted_at, handled_at, last_error, last_error_at, database_label, created_at, updated_at)
      SELECT
        uuid,
        name,
        input,
        hist.message_template,
        tries,
        max_tries,
        next_run_at,
        status,
        LEAST(last_error_at, next_run_at),
        last_error_at,
        last_error,
        IF(last_error IS NULL, NULL, last_error_at),
        SUBSTRING_INDEX(SUBSTRING_INDEX(`ctx`, '["pool","', - 1), '"]', 1),
        LEAST(last_error_at, next_run_at),
        GREATEST(last_error_at, next_run_at)
      FROM queue_jobs
      LEFT JOIN pool_message_history hist ON hist.queue_job_uuid = queue_jobs.uuid
      GROUP BY `uuid`
    |sql}
  |> Database.Migration.Step.create ~label:"define function to archive sihl job queue"
;;

let archive_history_name = "CopyRowsArchiveHistory"

let archive_queue_history_mappings_fcn =
  chunked
    archive_history_name
    "pool_message_history"
    {sql|
      INSERT INTO pool_queue_jobs_mapping (entity_uuid, queue_uuid, created_at, updated_at)
      SELECT
        entity_uuid,
        queue_job_uuid,
        created_at,
        updated_at
      FROM pool_message_history
    |sql}
  |> Database.Migration.Step.create ~label:"define function to archive sihl job mappings"
;;

let archive_queue_jobs_call name =
  Database.Migration.Step.create ~label:[%string "call function with %{name}"] (call name)
;;

let archive_queue_jobs_drop name =
  Database.Migration.Step.create ~label:[%string "drop function with %{name}"] (drop name)
;;

let drop_message_history_table =
  Database.Migration.Step.create
    ~label:"remove message history table"
    {sql| DROP TABLE IF EXISTS pool_message_history |sql}
;;

let drop_queue_jobs_table =
  Database.Migration.Step.create
    ~label:"remove sihl queue table"
    {sql| DROP TABLE IF EXISTS queue_jobs |sql}
;;

let drop_migration_state_from_sihl_queue =
  Database.Migration.Step.create
    ~label:"drop migration state of sihl queue"
    {sql| DELETE FROM core_migration_state WHERE namespace = 'queue' |sql}
;;

let move_where_fragment = "status = 'pending' AND tries != max_tries"

let move_still_pending_queue_jobs =
  Database.Migration.Step.create
    ~label:"move still pending queue jobs"
    [%string
      {sql|
      INSERT INTO pool_queue_jobs
        (uuid, name, input, message_template, tries, max_tries, run_at, status,
        persisted_at, handled_at, last_error, last_error_at, database_label, created_at, updated_at)
      SELECT
        uuid, name, input, message_template, tries, max_tries, run_at, status,
        persisted_at, handled_at, last_error, last_error_at, database_label, created_at, updated_at
      FROM pool_queue_jobs_history
      WHERE %{move_where_fragment}
    |sql}]
;;

let remove_still_pending_queue_jobs_from_history =
  Database.Migration.Step.create
    ~label:"remove still pending queue jobs from history"
    [%string
      {sql|
      DELETE FROM pool_queue_jobs_history
      WHERE %{move_where_fragment}
    |sql}]
;;

let set_failed_for_jobs =
  Database.Migration.Step.create
    ~label:"set failed for jobs"
    {sql|
      UPDATE pool_queue_jobs_history
      SET
        status = 'failed'
      WHERE status = 'pending'
        AND tries >= max_tries
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202407171415"
    |> add_step archive_queue_jobs_fcn
    |> add_step (archive_queue_jobs_call archive_jobs_name)
    |> add_step (archive_queue_jobs_drop archive_jobs_name)
    |> add_step archive_queue_history_mappings_fcn
    |> add_step (archive_queue_jobs_call archive_history_name)
    |> add_step (archive_queue_jobs_drop archive_history_name)
    |> add_step drop_message_history_table
    |> add_step drop_queue_jobs_table
    |> add_step drop_migration_state_from_sihl_queue
    |> add_step move_still_pending_queue_jobs
    |> add_step remove_still_pending_queue_jobs_from_history
    |> add_step set_failed_for_jobs)
;;
