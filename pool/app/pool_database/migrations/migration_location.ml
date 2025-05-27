let create_location_table =
  Database.Migration.Step.create
    ~label:"create locations table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_locations (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `name` varchar(255) NOT NULL,
        `description` text,
        `is_virtual` boolean,
        `room` varchar(255),
        `building` varchar(255),
        `street` varchar(255),
        `zip` varchar(255),
        `city` varchar(255),
        `link` varchar(255),
        `status` varchar(128) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_institution_to_address =
  Database.Migration.Step.create
    ~label:"add institution to address"
    {sql|
      ALTER TABLE pool_locations
        ADD COLUMN institution varchar(255) AFTER is_virtual
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "location"
    |> add_step create_location_table
    |> add_step add_institution_to_address)
;;
