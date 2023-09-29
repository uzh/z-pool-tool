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

let add_optional_mailing_uuid_field_to_invitation =
  Sihl.Database.Migration.create_step
    ~label:"add optional mailing uuid field to invitation"
    {sql|
      ALTER TABLE pool_invitations
        ADD COLUMN mailing_uuid binary(16) NULL DEFAULT NULL AFTER experiment_uuid,
        ADD CONSTRAINT fk_pool_invitations_mailing_uuid
          FOREIGN KEY (mailing_uuid) REFERENCES pool_mailing(uuid)
    |sql}
;;

let add_resend_count_field_to_invitation =
  Sihl.Database.Migration.create_step
    ~label:"add resend count field to invitation"
    {sql|
      ALTER TABLE pool_invitations
        ADD COLUMN resend_count integer NOT NULL DEFAULT 1 AFTER resent_at
    |sql}
;;

let update_resend_count_field_of_invitation =
  Sihl.Database.Migration.create_step
    ~label:"update resend count field of invitation"
    {sql|
      UPDATE pool_invitations
      SET resend_count = resend_count + 1
      WHERE resend_count = 1 AND resent_at IS NOT NULL
    |sql}
;;

let add_invitation_reset_at_to_experiment =
  Sihl.Database.Migration.create_step
    ~label:"add invitation reset at to experiment"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN invitation_reset_at datetime NULL DEFAULT NULL AFTER text_message_session_reminder_lead_time
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309281435"
    |> add_step create_pool_queue_jobs_mapping_table
    |> add_step add_optional_mailing_uuid_field_to_invitation
    |> add_step add_resend_count_field_to_invitation
    |> add_step update_resend_count_field_of_invitation
    |> add_step add_invitation_reset_at_to_experiment)
;;

let migration_root () =
  Sihl.Database.Migration.(
    empty "202309281435" |> add_step create_pool_queue_jobs_mapping_table)
;;
