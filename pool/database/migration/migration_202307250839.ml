let create_pool_experiments_participation_tags =
  Sihl.Database.Migration.create_step
    ~label:"create pool admins table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_experiments_participation_tags (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `experiment_uuid` binary(16) NOT NULL,
        `tag_uuid` binary(16) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `experiment_tag` (`experiment_uuid`, `tag_uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307250839" |> add_step create_pool_experiments_participation_tags)
;;
