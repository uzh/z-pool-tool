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

let add_location =
  Sihl.Database.Migration.create_step
    ~label:"add field for location"
    {sql|
      ALTER TABLE pool_sessions
      ADD COLUMN location_id bigint(20) unsigned NOT NULL AFTER description
    |sql}
;;

let add_follow_up =
  Sihl.Database.Migration.create_step
    ~label:"add follow up for sessions"
    {sql|
     ALTER TABLE pool_sessions
     ADD COLUMN follow_up_to binary(16)
     AFTER uuid
    |sql}
;;

let add_reminder_columns =
  Sihl.Database.Migration.create_step
    ~label:"add field versioning for participants"
    {sql|
     ALTER TABLE pool_sessions
      ADD COLUMN reminder_subject TEXT DEFAULT NULL AFTER overbook,
      ADD COLUMN reminder_text TEXT DEFAULT NULL AFTER reminder_subject,
      ADD COLUMN reminder_lead_time INTEGER DEFAULT NULL AFTER reminder_text,
      ADD COLUMN reminder_sent_at timestamp NULL AFTER reminder_lead_time
     |sql}
;;

let add_closed_at_column =
  Sihl.Database.Migration.create_step
    ~label:"add closed at column"
    {sql|
     ALTER TABLE pool_sessions
      ADD COLUMN closed_at TIMESTAMP NULL AFTER reminder_sent_at
     |sql}
;;

let remove_message_columns =
  Sihl.Database.Migration.create_step
    ~label:"rename filter column"
    {sql|
      ALTER TABLE pool_sessions
      DROP COLUMN reminder_subject,
      DROP COLUMN reminder_text
    |sql}
;;

let drop_start_default_value =
  Sihl.Database.Migration.create_step
    ~label:"rename filter column"
    {sql|
      ALTER TABLE pool_sessions
        MODIFY COLUMN `start` TIMESTAMP NULL;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "session"
    |> add_step create_participant_table
    |> add_step add_location
    |> add_step add_follow_up
    |> add_step add_reminder_columns
    |> add_step add_closed_at_column
    |> add_step drop_start_default_value)
;;
