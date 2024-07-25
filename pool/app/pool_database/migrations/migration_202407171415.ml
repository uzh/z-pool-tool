let archive_queue_jobs =
  Database.Migration.Step.create
    ~label:"archive sihl job queue"
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
;;

let archive_queue_history_mappings =
  Database.Migration.Step.create
    ~label:"archive sihl job queue"
    {sql|
      INSERT INTO pool_queue_jobs_mapping (entity_uuid, queue_uuid, created_at, updated_at)
      SELECT
        entity_uuid,
        queue_job_uuid,
        created_at,
        updated_at
      FROM pool_message_history
    |sql}
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

let migration () =
  Database.Migration.(
    empty "202407171415"
    |> add_step archive_queue_jobs
    |> add_step archive_queue_history_mappings
    |> add_step drop_message_history_table
    |> add_step drop_queue_jobs_table
    |> add_step drop_migration_state_from_sihl_queue)
;;
