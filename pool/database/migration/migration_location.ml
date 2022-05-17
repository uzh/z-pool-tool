let create_location_table =
  Sihl.Database.Migration.create_step
    ~label:"create locations table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_locations (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `name` varchar(255) NOT NULL,
        `description` text,
        `room` varchar(255) NOT NULL,
        `building` varchar(255) NOT NULL,
        `street` varchar(255) NOT NULL,
        `zip` varchar(255) NOT NULL,
        `city` varchar(255) NOT NULL,
        `link` varchar(255) NOT NULL,
        `status` varchar(128) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(empty "location" |> add_step create_location_table)
;;
