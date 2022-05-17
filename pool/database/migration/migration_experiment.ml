let create_pool_experiments_table =
  Sihl.Database.Migration.create_step
    ~label:"create pool_experiments table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_experiments (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `title` varchar(255) NOT NULL,
        `description` varchar(255),
        `filter` TEXT,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_waiting_list_flags_to_experiment =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN direct_registration_disabled boolean NOT NULL DEFAULT 0 AFTER filter,
        ADD COLUMN waiting_list_disabled boolean NOT NULL DEFAULT 0 AFTER filter
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "pool_experiments"
    |> add_step create_pool_experiments_table
    |> add_step add_waiting_list_flags_to_experiment)
;;
