let create_pool_message_table =
  Sihl.Database.Migration.create_step
    ~label:"create pool message table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_message_templates (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `label` varchar(255) NOT NULL,
        `entity_uuid` binary(16),
        `language` varchar(128) NOT NULL,
        `email_subject` varchar(255) NOT NULL,
        `email_text_html` text NOT NULL,
        `sms_text` text,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "pool_message" |> add_step create_pool_message_table)
;;
