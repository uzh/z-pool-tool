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

let migration () =
  Sihl.Database.Migration.(
    empty "session"
    |> add_step create_participant_table
    |> add_step add_location
    |> add_step add_follow_up)
;;
