let create_tag_table =
  Sihl.Database.Migration.create_step
    ~label:"create tag table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tags (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `title` varchar(255) NOT NULL,
        `description` text,
        `model` varchar(255) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_model_title` (`title`, `model`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let create_tagging_table =
  Sihl.Database.Migration.create_step
    ~label:"create tagging table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tagging (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `model_uuid` binary(16) NOT NULL,
        `tag_uuid` binary(16) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_model_tag` (`model_uuid`, `tag_uuid`),
      FOREIGN KEY (tag_uuid) REFERENCES pool_tags(uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307121330"
    |> add_step create_tag_table
    |> add_step create_tagging_table)
;;
