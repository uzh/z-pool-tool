let create_custom_fields_table =
  Database.Migration.create_step
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

let add_admin_boolean_columns =
  Database.Migration.create_step
    ~label:"add admin boolean fields to custom fields table"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN admin_view_only boolean NOT NULL DEFAULT 0 AFTER admin_overwrite,
        ADD COLUMN admin_input_only boolean NOT NULL DEFAULT 0 AFTER admin_view_only
    |sql}
;;

let add_group_column_to_custom_fields =
  Database.Migration.create_step
    ~label:"add group column to custom fields"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN custom_field_group_uuid binary(16) AFTER disabled
    |sql}
;;

let add_position_to_custom_fields =
  Database.Migration.create_step
    ~label:"add position to custom fields"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN position tinyint DEFAULT 0 AFTER admin_input_only
    |sql}
;;

let change_position_datatype =
  Database.Migration.create_step
    ~label:"change position datatype"
    {sql|
      ALTER TABLE pool_custom_fields
        MODIFY position int
    |sql}
;;

let add_published_at_to_custom_fields =
  Database.Migration.create_step
    ~label:"add published at to custom fields"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN published_at timestamp NULL AFTER position
    |sql}
;;

let rename_overwrite_to_override =
  Database.Migration.create_step
    ~label:"rename overwrite to override"
    {sql|
      ALTER TABLE pool_custom_fields
        RENAME COLUMN admin_overwrite TO admin_override
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_fields"
    |> add_step create_custom_fields_table
    |> add_step add_admin_boolean_columns
    |> add_step add_group_column_to_custom_fields
    |> add_step add_position_to_custom_fields
    |> add_step change_position_datatype
    |> add_step add_published_at_to_custom_fields
    |> add_step rename_overwrite_to_override)
;;
