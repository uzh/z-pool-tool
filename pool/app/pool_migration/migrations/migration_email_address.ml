let create_email_address_table =
  Database.Migration.create_step
    ~label:"create email address table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_email_verifications (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `address` varchar(128) NOT NULL,
        `user_uuid` binary(16) NOT NULL,
        `token` varchar(128) NOT NULL,
        `verified` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "email_address" |> add_step create_email_address_table)
;;
