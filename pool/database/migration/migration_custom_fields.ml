let create_custom_fields_table =
  Sihl.Database.Migration.create_step
    ~label:"create custom fields table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_custom_fields (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `model` varchar(255) NOT NULL,
        `name` text NOT NULL,
        `hint` text NOT NULL,
        `field_type` varchar(128) NOT NULL,
        `validation` text,
        `required` boolean NOT NULL DEFAULT 0,
        `disabled` boolean NOT NULL DEFAULT 0,
        `admin_hint` text DEFAULT NULL,
        `admin_overwrite` boolean NOT NULL DEFAULT 0,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_fields" |> add_step create_custom_fields_table)
;;
