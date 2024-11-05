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
        `target_user_uuid` binary(16) NOT NULL,
        `contact_uuid` binary(16) NOT NULL,
        `score` float NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_target_user_combination` (`target_user_uuid`, `contact_uuid`),
      FOREIGN KEY (`target_user_uuid`) REFERENCES user_users(`uuid`),
      FOREIGN KEY (`contact_uuid`) REFERENCES user_users(`uuid`)
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

let migration () =
  Database.Migration.(
    empty "202411011201"
    |> add_step create_custom_field_answer_index
    |> add_step create_duplicates_table
    |> add_step add_weight_to_custom_fields)
;;

(* TODO: Two way uniqueness: 

- manually check? number of new duplicates should become small
- https://chatgpt.com/c/672a0605-7f94-8005-b01c-bcca59f498cf
- https://stackoverflow.com/questions/41040301/bi-directional-unique-key-constraint-for-combination-of-two-columns

*)
