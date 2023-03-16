let create_participation_table =
  Sihl.Database.Migration.create_step
    ~label:"create participation table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_participations (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `session_id` bigint(20) NOT NULL,
        `participant_id` bigint(20) NOT NULL,
        `show_up` boolean NOT NULL,
        `participated` boolean NOT NULL,
        `matches_filter` boolean NOT NULL,
        `canceled_at` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let rename_participant_to_subject =
  Sihl.Database.Migration.create_step
    ~label:"rename participant id to subject id"
    {sql|
      ALTER TABLE pool_participations
        RENAME COLUMN participant_id TO subject_id
    |sql}
;;

let rename_subject_to_contact =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_participations
        RENAME COLUMN subject_id TO contact_id
    |sql}
;;

let rename_table_to_assignments =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_participations
        RENAME pool_assignments
    |sql}
;;

let make_show_up_participated_nullable =
  Sihl.Database.Migration.create_step
    ~label:"make show_up and participated nullable"
    {sql|
      ALTER TABLE pool_assignments
        MODIFY COLUMN show_up BOOLEAN NULL,
        MODIFY COLUMN participated BOOLEAN
    |sql}
;;

let add_marked_as_deleted_column =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_assignments
        ADD COLUMN marked_as_deleted boolean DEFAULT 0 AFTER canceled_at
    |sql}
;;

let replace_ids_with_uuids_as_foreignkeys =
  Sihl.Database.Migration.create_step
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
  Sihl.Database.Migration.(
    empty "participation"
    |> add_step create_participation_table
    |> add_step rename_participant_to_subject
    |> add_step rename_subject_to_contact
    |> add_step rename_table_to_assignments
    |> add_step make_show_up_participated_nullable
    |> add_step add_marked_as_deleted_column
    |> add_step replace_ids_with_uuids_as_foreignkeys)
;;
