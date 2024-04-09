let create_filter_table =
  Database.Migration.create_step
    ~label:"create filter table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_filter (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `filter` text NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_title_to_filter =
  Database.Migration.create_step
    ~label:"add title to filter"
    {sql|
      ALTER TABLE pool_filter
        ADD COLUMN title varchar(255) AFTER filter
    |sql}
;;

let rename_filter_column =
  Database.Migration.create_step
    ~label:"rename filter column"
    {sql|
      ALTER TABLE pool_filter
        RENAME COLUMN filter TO query
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "filter"
    |> add_step create_filter_table
    |> add_step add_title_to_filter
    |> add_step rename_filter_column)
;;
