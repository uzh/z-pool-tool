let create_custom_field_groups_table =
  Database.Migration.Step.create
    ~label:"create custom field groups table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_custom_field_groups (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `model` varchar(255) NOT NULL,
        `name` text NOT NULL,
        `position` tinyint DEFAULT 0,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let change_position_datatype =
  Database.Migration.Step.create
    ~label:"change position datatype"
    {sql|
      ALTER TABLE pool_custom_field_groups
        MODIFY position int
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_field_groups"
    |> add_step create_custom_field_groups_table
    |> add_step change_position_datatype)
;;
