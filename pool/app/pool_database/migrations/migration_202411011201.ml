let create_custom_field_answer_index =
  Database.Migration.Step.create
    ~label:"create custom_field_answer index"
    {sql|
      CREATE INDEX entity_uuid_custom_field_uuid ON pool_custom_field_answers(entity_uuid, custom_field_uuid);
    |sql}
;;

let create_duplicates_table =
  Database.Migration.Step.create
    ~label:"create duplicates table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_contacts_possible_duplicates (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `contact_a` binary(16) NOT NULL,
        `contact_b` binary(16) NOT NULL,
        `score` float NOT NULL,
        `ignore` boolean DEFAULT 0 NOT NULL,
        `unique_combination` binary(16) GENERATED ALWAYS AS (UNHEX(CONCAT(contact_a, contact_b))) STORED,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_combination` (`unique_combination`),
      FOREIGN KEY (`contact_a`) REFERENCES user_users(`uuid`),
      FOREIGN KEY (`contact_b`) REFERENCES user_users(`uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_weight_to_custom_fields =
  Database.Migration.Step.create
    ~label:"add weight to custom_fields"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN possible_duplicates_weight SMALLINT UNSIGNED DEFAULT 0 AFTER published_at
    |sql}
;;

let create_duplicate_contact_permissions =
  Database.Migration.Step.create
    ~label:"add duplicate_contacts permission"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Operator', 'manage', '`DuplicateContact')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202411011201"
    |> add_step create_custom_field_answer_index
    |> add_step create_duplicates_table
    |> add_step add_weight_to_custom_fields
    |> add_step create_duplicate_contact_permissions)
;;
