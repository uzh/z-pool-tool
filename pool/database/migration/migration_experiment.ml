let create_pool_experiments_table =
  Sihl.Database.Migration.create_step
    ~label:"create pool_experiments table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_experiments (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `title` varchar(255) NOT NULL,
        `description` varchar(255),
        `filter` TEXT,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_waiting_list_flags_to_experiment =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN direct_registration_disabled boolean NOT NULL DEFAULT 0 AFTER filter,
        ADD COLUMN waiting_list_disabled boolean NOT NULL DEFAULT 0 AFTER filter
    |sql}
;;

let change_description_column_type =
  Sihl.Database.Migration.create_step
    ~label:"change description column type"
    {sql|
      ALTER TABLE pool_experiments
        MODIFY description text
    |sql}
;;

let add_disable_registration =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN registration_disabled boolean NOT NULL DEFAULT 0 AFTER direct_registration_disabled
    |sql}
;;

let merge_waiting_list_flags =
  Sihl.Database.Migration.create_step
    ~label:"merge waiting list flags"
    {sql|
      ALTER TABLE pool_experiments
        DROP COLUMN waiting_list_disabled
    |sql}
;;

let add_public_title =
  Sihl.Database.Migration.create_step
    ~label:"add public title"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN public_title varchar(255) NOT NULL AFTER title
    |sql}
;;

let set_default_public_title =
  Sihl.Database.Migration.create_step
    ~label:"set default public title"
    {sql|
      UPDATE pool_experiments SET public_title = title
        WHERE public_title is NULL OR public_title = ''
    |sql}
;;

let add_session_reminder_columns =
  Sihl.Database.Migration.create_step
    ~label:"add field versioning for participants"
    {sql|
     ALTER TABLE pool_experiments
     ADD COLUMN session_reminder_lead_time INTEGER DEFAULT NULL AFTER description,
     ADD COLUMN session_reminder_subject TEXT DEFAULT NULL AFTER session_reminder_lead_time,
     ADD COLUMN session_reminder_text TEXT DEFAULT NULL AFTER session_reminder_subject
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "pool_experiments"
    |> add_step create_pool_experiments_table
    |> add_step add_waiting_list_flags_to_experiment
    |> add_step change_description_column_type
    |> add_step add_disable_registration
    |> add_step merge_waiting_list_flags
    |> add_step add_public_title
    |> add_step set_default_public_title
    |> add_step add_session_reminder_columns)
;;
