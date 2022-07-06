let create_mailing_table =
  Sihl.Database.Migration.create_step
    ~label:"create mailing table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_mailing (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `experiment_id` varchar(255) NOT NULL,
        `start` timestamp NOT NULL,
        `end` timestamp NOT NULL,
        `rate` integer NOT NULL,
        `distribution` varchar(512),
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(empty "mailing" |> add_step create_mailing_table)
;;
