let create_system_settings_table =
  Database.Migration.Step.create
    ~label:"create system_settings table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_system_settings (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `settings_key` varchar(128) NOT NULL,
        `value` text NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Database.Migration.(empty "system_settings" |> add_step create_system_settings_table)
;;
