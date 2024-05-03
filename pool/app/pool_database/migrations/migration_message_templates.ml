let create_pool_message_table =
  Database.Migration.Step.create
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
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_label_entity_id_language` (`label`, `entity_uuid`, `language`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_plain_text_column =
  Database.Migration.Step.create
    ~label:"add plain text column"
    {sql|
      ALTER TABLE pool_message_templates
        ADD COLUMN email_text_plain text AFTER email_text_html
    |sql}
;;

let set_plain_text_to_sms_text =
  Database.Migration.Step.create
    ~label:"set plain text to sms text"
    {sql|
      UPDATE pool_message_templates
        SET email_text_plain = sms_text
    |sql}
;;

let make_plain_text_not_nullable =
  Database.Migration.Step.create
    ~label:"make plain text not nullable"
    {sql|
    ALTER TABLE pool_message_templates
        MODIFY email_text_plain text NOT NULL
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "pool_message"
    |> add_step create_pool_message_table
    |> add_step add_plain_text_column
    |> add_step set_plain_text_to_sms_text
    |> add_step make_plain_text_not_nullable)
;;
