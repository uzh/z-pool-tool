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

let add_order_column_to_custom_field_options =
  Sihl.Database.Migration.create_step
    ~label:"add order column to custom field options table"
    {sql|
      ALTER TABLE pool_custom_field_options
        ADD COLUMN position tinyint DEFAULT 0 AFTER name
    |sql}
;;

let change_position_datatype =
  Sihl.Database.Migration.create_step
    ~label:"change position datatype"
    {sql|
      ALTER TABLE pool_custom_field_options
        MODIFY position int
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_field_options"
    |> add_step create_custom_field_options_table
    |> add_step add_order_column_to_custom_field_options
    |> add_step change_position_datatype)
;;
