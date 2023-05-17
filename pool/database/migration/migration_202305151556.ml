let create_phone_number_verifications_table =
  Sihl.Database.Migration.create_step
    ~label:"create phone number verifications table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_phone_number_verifications (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `phone_number` varchar(128) NOT NULL,
        `user_uuid` binary(16) NOT NULL,
        `token` varchar(128) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_contact_uuid` (`user_uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_phone_number_column_to_contact_table =
  Sihl.Database.Migration.create_step
    ~label:"add phone number column to contact table"
    {sql|
      ALTER TABLE pool_contacts
        ADD COLUMN phone_number varchar(128) AFTER language
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202305151556"
    |> add_step create_phone_number_verifications_table
    |> add_step add_phone_number_column_to_contact_table)
;;
