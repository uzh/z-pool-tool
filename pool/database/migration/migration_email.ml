let create_email_address_table =
  Sihl.Database.Migration.create_step
    ~label:"create email address table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_emails (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `user_uuid` binary(16) NOT NULL,
        `address` varchar(128) NOT NULL,
        `token` timestamp NULL,
        `verified` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`user_uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "email_address" |> add_step create_email_address_table)
;;
