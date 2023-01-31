let create_i18n_table =
  Sihl.Database.Migration.create_step
    ~label:"create i18n table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_i18n (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `i18n_key` varchar(128) NOT NULL,
        `language` varchar(128) NOT NULL,
        `content` text,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let remove_unused_rows =
  Sihl.Database.Migration.create_step
    ~label:"remove unused rows from i18n table"
    {sql|
      DELETE FROM pool_i18n
      WHERE i18n_key IN ('confirmation_subject', 'confirmation_text')
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(empty "i18n" |> add_step create_i18n_table)
;;
