let create_custom_field_answers_table =
  Database.Migration.Step.create
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
  Database.Migration.Step.create
    ~label:"remove version column"
    {sql|
      ALTER TABLE pool_custom_field_answers
        DROP COLUMN version
    |sql}
;;

let make_value_nullable =
  Database.Migration.Step.create
    ~label:"remove version column"
    {sql|
      ALTER TABLE pool_custom_field_answers
        MODIFY COLUMN value text
    |sql}
;;

(* To keep the migrations clean, I added a default value (Versions only matter when
   mutliple people are updating) *)
let add_versions_and_admin_values =
  Database.Migration.Step.create
    ~label:"add versions and admin values"
    {sql|
      ALTER TABLE pool_custom_field_answers
        ADD COLUMN admin_value text after value,
        ADD COLUMN version bigint(20) DEFAULT 0 NOT NULL AFTER admin_value,
        ADD COLUMN admin_version bigint(20) DEFAULT 0 NOT NULL AFTER version
    |sql}
;;

let add_unique_combination_constraint =
  Database.Migration.Step.create
    ~label:"add unique combination constraint to answers"
    {sql|
      ALTER TABLE pool_custom_field_answers
        ADD CONSTRAINT field_entity_combination
          UNIQUE (custom_field_uuid, entity_uuid)
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "custom_field_answers"
    |> add_step create_custom_field_answers_table
    |> add_step remove_version_column
    |> add_step make_value_nullable
    |> add_step add_versions_and_admin_values
    |> add_step add_unique_combination_constraint)
;;
