let create_changelog_table =
  Database.Migration.Step.create
    ~label:"create changelog table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_change_log (
        `id` BIGINT UNSIGNED AUTO_INCREMENT,
        `uuid` BINARY(16) NOT NULL,
        `model` VARCHAR(128) NOT NULL,
        `entity_uuid` BINARY(16) NOT NULL,
        `user_uuid` BINARY(16),
        `changes` TEXT NOT NULL DEFAULT '',
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      CONSTRAINT unique_uuid UNIQUE KEY (uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

(* TODO: Fix manually on staging *)
let add_user_users_uuid_index =
  Database.Migration.Step.create
    ~label:"add user_users uuid index"
    {sql| 
      CREATE INDEX user_users_uuid_index ON user_users (uuid);
    |sql}
;;

let add_fk_contraint_to_users =
  Database.Migration.Step.create
    ~label:"add fk contraint to users"
    {sql| 
      ALTER TABLE pool_change_log ADD CONSTRAINT `fk_pool_changelog_user_users` 
        FOREIGN KEY (user_uuid) REFERENCES user_users(uuid)
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202408081359"
    |> add_step create_changelog_table
    |> add_step add_user_users_uuid_index
    |> add_step add_fk_contraint_to_users)
;;
