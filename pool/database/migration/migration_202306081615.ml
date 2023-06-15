let create_organisational_units_table =
  Sihl.Database.Migration.create_step
    ~label:"create organisational units table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_organisational_units (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `name` varchar(255) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_missing_columns_to_experiments =
  Sihl.Database.Migration.create_step
    ~label:"add missing columns to experiments"
    {sql|
    ALTER TABLE pool_experiments
      ADD COLUMN organisational_unit_uuid binary(16) AFTER filter_uuid,
      ADD COLUMN allow_blank_mri_key boolean DEFAULT 0 AFTER organisational_unit_uuid,
      ADD COLUMN cost_center varchar(255) AFTER session_reminder_lead_time,
      ADD COLUMN closed boolean DEFAULT 0 AFTER direct_registration_disabled,
      ADD COLUMN hide_in_calendar boolean DEFAULT 0 AFTER experiment_type,
      ADD COLUMN hide_in_stats boolean DEFAULT 0 AFTER hide_in_calendar
    |sql}
;;

let add_fk_contraint_to_organisational_units =
  Sihl.Database.Migration.create_step
    ~label:"add unique contraint to organisational_units"
    {sql|
      ALTER TABLE pool_experiments
        ADD CONSTRAINT fk_pool_experiments_organisational_units
        FOREIGN KEY (organisational_unit_uuid) REFERENCES pool_organisational_units(uuid)
    |sql}
;;

let add_missing_columns_to_sessions =
  Sihl.Database.Migration.create_step
    ~label:"add missing columns to sessions"
    {sql|
    ALTER TABLE pool_sessions
      ADD COLUMN limitations text AFTER description
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202306081615"
    |> add_step create_organisational_units_table
    |> add_step add_missing_columns_to_experiments
    |> add_step add_fk_contraint_to_organisational_units
    |> add_step add_missing_columns_to_sessions)
;;
