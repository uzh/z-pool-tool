let create_participant_table =
  Database.Migration.Step.create
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
  Database.Migration.Step.create
    ~label:"add field for location"
    {sql|
      ALTER TABLE pool_sessions
      ADD COLUMN location_id bigint(20) unsigned NOT NULL AFTER description
    |sql}
;;

let add_follow_up =
  Database.Migration.Step.create
    ~label:"add follow up for sessions"
    {sql|
     ALTER TABLE pool_sessions
     ADD COLUMN follow_up_to binary(16)
     AFTER uuid
    |sql}
;;

let add_reminder_columns =
  Database.Migration.Step.create
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
  Database.Migration.Step.create
    ~label:"add closed at column"
    {sql|
     ALTER TABLE pool_sessions
      ADD COLUMN closed_at TIMESTAMP NULL AFTER reminder_sent_at
     |sql}
;;

let remove_message_columns =
  Database.Migration.Step.create
    ~label:"rename filter column"
    {sql|
      ALTER TABLE pool_sessions
      DROP COLUMN reminder_subject,
      DROP COLUMN reminder_text
    |sql}
;;

let drop_start_default_value =
  Database.Migration.Step.create
    ~label:"drop start default value"
    {sql|
      ALTER TABLE pool_sessions
        MODIFY COLUMN `start` TIMESTAMP NULL;
    |sql}
;;

let use_uuids_as_foreign_keys_in_assignment_table =
  Database.Migration.Step.create
    ~label:"replace ids with uuids as foreignkeys"
    {sql|
      BEGIN NOT ATOMIC
        ALTER TABLE pool_assignments
          ADD COLUMN session_uuid binary(16) AFTER session_id,
          ADD COLUMN contact_uuid binary(16) AFTER contact_id;

        UPDATE pool_assignments SET
          session_uuid = (SELECT uuid FROM pool_sessions WHERE pool_sessions.id = pool_assignments.session_id),
          contact_uuid = (SELECT user_uuid FROM pool_contacts WHERE pool_contacts.id = pool_assignments.contact_id);

        ALTER TABLE pool_assignments
          ADD CONSTRAINT unique_session_contact_combination UNIQUE (session_uuid, contact_uuid);

        ALTER TABLE pool_assignments
          DROP COLUMN session_id,
          DROP COLUMN contact_id;
      END;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "session"
    |> add_step create_participant_table
    |> add_step add_location
    |> add_step add_follow_up
    |> add_step add_reminder_columns
    |> add_step add_closed_at_column
    |> add_step drop_start_default_value
    |> add_step use_uuids_as_foreign_keys_in_assignment_table)
;;
