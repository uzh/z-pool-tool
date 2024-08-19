let queue_table name =
  [%string
    {sql|
      CREATE TABLE IF NOT EXISTS %{name} (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `name` varchar(128) NOT NULL,
        `input` text NOT NULL,
        `message_template` text NULL,
        `tries` SMALLINT UNSIGNED NOT NULL,
        `max_tries` SMALLINT UNSIGNED NOT NULL,
        `run_at` datetime NOT NULL,
        `status` varchar(128) NOT NULL DEFAULT 'active',
        `persisted_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `polled_at` datetime,
        `handled_at` datetime,
        `last_error` text,
        `last_error_at` datetime,
        `database_label` varchar(255) NOT NULL,
        `clone_of` binary(16),
        `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}]
;;

let add_queue_jobs_table =
  queue_table "pool_queue_jobs"
  |> Database.Migration.Step.create ~label:"add queue jobs table"
;;

let add_queue_jobs_history_table =
  queue_table "pool_queue_jobs_history"
  |> Database.Migration.Step.create ~label:"add queue jobs history table"
;;

let add_queue_jobs_mapping_table =
  {sql|
    CREATE TABLE IF NOT EXISTS pool_queue_jobs_mapping (
      `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      `entity_uuid` binary(16) NOT NULL,
      `queue_uuid` binary(16) NOT NULL,
      `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
      `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (id)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
  |> Database.Migration.Step.create ~label:"add queue jobs mapping table"
;;

let migration () =
  Database.Migration.(
    empty "202406051700"
    |> add_step add_queue_jobs_table
    |> add_step add_queue_jobs_history_table
    |> add_step add_queue_jobs_mapping_table)
;;
