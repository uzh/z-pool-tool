let create_system_events_table =
  Sihl.Database.Migration.create_step
    ~label:"create system events table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_system_events (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `key` varchar(255) NOT NULL,
        `argument` text NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let create_system_event_logs_table =
  Sihl.Database.Migration.create_step
    ~label:"create system event logs table"
    (* TODO: rename hostname to service_identifier *)
    {sql|
      CREATE TABLE IF NOT EXISTS pool_system_event_logs (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `event_uuid` binary(16) NOT NULL,
        `hostname` varchar(255) NOT NULL,
        `status` ENUM('failed', 'successful') NOT NULL,
        `message` text NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202306021033" |> add_step create_system_events_table)
;;
