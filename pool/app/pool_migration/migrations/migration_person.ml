let create_person_table =
  Database.Migration.Step.create
    ~label:"create person table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_person (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `role` varchar(255) NOT NULL,
        `sihl_user_uuid` binary(16) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_sihl_user_uuid` (`sihl_user_uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let drop_person_table =
  Database.Migration.Step.create
    ~label:"drop person table"
    {sql|DROP TABLE IF EXISTS pool_person|sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "person" |> add_step create_person_table |> add_step drop_person_table)
;;
