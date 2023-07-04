let create_pool_admins_table =
  Sihl.Database.Migration.create_step
    ~label:"create pool admins table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_admins (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `user_uuid` binary(16) NOT NULL,
        `sign_in_count` integer DEFAULT 0,
        `last_sign_in_at` timestamp NULL,
        `import_pending` boolean NOT NULL default 0,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_admin_uuid` (`user_uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let seed_pool_admins_table_from_user_users_table =
  Sihl.Database.Migration.create_step
    ~label:"seed pool admins table from user users table"
    {sql|
      INSERT INTO pool_admins (user_uuid, created_at, updated_at)
      SELECT uuid, created_at, updated_at
      FROM user_users
      WHERE user_users.admin = 1;
    |sql}
;;

let create_user_imports_table =
  Sihl.Database.Migration.create_step
    ~label:"create user imports table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_user_imports (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `user_uuid` binary(16) NOT NULL,
        `token` varchar(128) NOT NULL,
        `confirmed_at` timestamp NULL,
        `notification_sent_at` timestamp NULL,
        `reminder_sent_at` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_user_uuid` (`user_uuid`),
      FOREIGN KEY (user_uuid) REFERENCES user_users (`uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_import_flag_to_contacts_table =
  Sihl.Database.Migration.create_step
    ~label:"add import fk to contacts table"
    {sql|
      ALTER TABLE pool_contacts
        ADD COLUMN import_pending boolean NOT NULL DEFAULT 0 AFTER profile_update_triggered_at
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307131619"
    |> add_step create_pool_admins_table
    |> add_step seed_pool_admins_table_from_user_users_table
    |> add_step create_user_imports_table
    |> add_step add_import_flag_to_contacts_table)
;;
