let create_mappings_table =
  Database.Migration.Step.create
    ~label:"create location_file_mappings table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_location_file_mappings (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `label` varchar(255) NOT NULL,
        `language` varchar(128) NOT NULL,
        `asset_id` bigint(20) unsigned NOT NULL,
        `location_id` bigint(20) unsigned NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "location_file_mapping" |> add_step create_mappings_table)
;;
