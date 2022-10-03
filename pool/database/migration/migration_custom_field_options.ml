let create_custom_field_options_table =
  Sihl.Database.Migration.create_step
    ~label:"create custom field options table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_custom_field_options (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `custom_field_option_uuid` binary(16) NOT NULL,
        `name` text NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_field_options" |> add_step create_custom_field_options_table)
;;
