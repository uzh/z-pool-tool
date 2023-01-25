let create_custom_field_answers_table =
  Sihl.Database.Migration.create_step
    ~label:"create custom field answers table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_custom_field_answers (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `custom_field_uuid` binary(16) NOT NULL,
        `entity_uuid` binary(16) NOT NULL,
        `value` text NOT NULL,
        `version` bigint(20) NOT NULL DEFAULT 0,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let remove_version_column =
  Sihl.Database.Migration.create_step
    ~label:"remove version column"
    {sql|
      ALTER TABLE pool_custom_field_answers
        DROP COLUMN version
    |sql}
;;

let make_value_nullable =
  Sihl.Database.Migration.create_step
    ~label:"remove version column"
    {sql|
      ALTER TABLE pool_custom_field_answers
        MODIFY COLUMN value text
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_field_answers"
    |> add_step create_custom_field_answers_table
    |> add_step remove_version_column
    |> add_step make_value_nullable)
;;
