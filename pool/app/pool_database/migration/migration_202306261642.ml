let create_failed_login_attempts_table =
  Sihl.Database.Migration.create_step
    ~label:"create failed login attempts table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_failed_login_attempts (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `email` varchar(128) NOT NULL,
        `counter` integer NOT NULL DEFAULT 0,
        `blocked_until` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_email` (`email`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202306261642" |> add_step create_failed_login_attempts_table)
;;

let migration_root () =
  Sihl.Database.Migration.(
    empty "202306261642" |> add_step create_failed_login_attempts_table)
;;
