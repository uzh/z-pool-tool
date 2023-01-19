let create_custom_field_answer_versions_table =
  Sihl.Database.Migration.create_step
    ~label:"create custom field answer versions table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_custom_field_answer_versions (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `custom_field_uuid` binary(16) NOT NULL,
        `entity_uuid` binary(16) NOT NULL,
        `version` bigint(20) NOT NULL DEFAULT 0,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_field_and_entity` (`custom_field_uuid`, `entity_uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_unique_combination_constraint =
  Sihl.Database.Migration.create_step
    ~label:"remove version column"
    {sql|
      ALTER TABLE pool_custom_field_answer_versions
        ADD CONSTRAINT field_entity_combination
          UNIQUE (custom_field_uuid, entity_uuid)
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "custom_field_answer_versions"
    |> add_step create_custom_field_answer_versions_table
    |> add_step add_unique_combination_constraint)
;;
