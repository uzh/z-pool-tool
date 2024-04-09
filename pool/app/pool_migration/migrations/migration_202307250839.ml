let create_pool_participation_tags =
  Database.Migration.create_step
    ~label:"create pool admins table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_participation_tags (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `entity_uuid` binary(16) NOT NULL,
        `tag_uuid` binary(16) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `entity_tag` (`entity_uuid`, `tag_uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307250839" |> add_step create_pool_participation_tags)
;;
