let create_pool_queue_jobs_mapping_table =
  Sihl.Database.Migration.create_step
    ~label:"create pool queue jobs mapping table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_queue_jobs_mapping (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `queue_uuid` binary(16) NOT NULL,
        `entity` varchar(255) NOT NULL,
        `entity_uuid` binary(16) NOT NULL,
        `tag_uuid` binary(16) NOT NULL,
        `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `queue_entity_uuid` (`queue_uuid`, `entity`, `entity_uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_limit_to_mailing =
  Sihl.Database.Migration.create_step
    ~label:"add limit to mailing"
    {sql|
      ALTER TABLE pool_mailing
        ADD COLUMN `limit` integer NOT NULL DEFAULT 0 AFTER rate
    |sql}
;;

let change_mailing_rate_to_limit =
  Sihl.Database.Migration.create_step
    ~label:"change mailing rate to limit"
    {sql|
      UPDATE pool_mailing
      SET
        `limit` = ROUND(TIMESTAMPDIFF(MINUTE, `start`, `end`) / 60 * rate)
      WHERE rate != 0 AND `start` < `end`;
    |sql}
;;

let drop_rate_from_mailing =
  Sihl.Database.Migration.create_step
    ~label:"drop rate from mailing"
    {sql|
      ALTER TABLE pool_mailing
      DROP COLUMN rate
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202310191345"
    |> add_step create_pool_queue_jobs_mapping_table
    |> add_step add_limit_to_mailing
    |> add_step change_mailing_rate_to_limit
    |> add_step drop_rate_from_mailing)
;;
