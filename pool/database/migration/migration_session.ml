let create_participant_table =
  Sihl.Database.Migration.create_step
    ~label:"create session table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_sessions (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `experiment_uuid` binary(16) NOT NULL,
        `start` timestamp NOT NULL,
        `duration` integer NOT NULL,
        `description` varchar(512) NULL,
        `max_participants` SMALLINT UNSIGNED NOT NULL,
        `min_participants` SMALLINT UNSIGNED NOT NULL,
        `overbook` SMALLINT UNSIGNED NOT NULL,
        `canceled_at` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_reminder_columns =
  Sihl.Database.Migration.create_step
    ~label:"add field versioning for participants"
    {sql|
     ALTER TABLE pool_sessions
     ADD COLUMN reminder_text TEXT DEFAULT NULL AFTER overbook,
     ADD COLUMN reminder_lead_time INTEGER DEFAULT NULL AFTER reminder_text
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "session"
    |> add_step create_participant_table
    |> add_step add_reminder_columns)
;;
