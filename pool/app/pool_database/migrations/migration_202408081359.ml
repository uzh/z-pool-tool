let create_changelog_table =
  Database.Migration.Step.create
    ~label:"create changelog table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_change_log (
        `id` BIGINT UNSIGNED AUTO_INCREMENT,
        `uuid` BINARY(16) NOT NULL,
        `model` VARCHAR(128) NOT NULL,
        `user_uuid` BINARY(16) NOT NULL,
        `changes` TEXT NOT NULL DEFAULT '',
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      CONSTRAINT unique_uuid UNIQUE KEY (uuid),
      FOREIGN KEY (user_uuid) REFERENCES user_users (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Database.Migration.(empty "202408081359" |> add_step create_changelog_table)
;;
