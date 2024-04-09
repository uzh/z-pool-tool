let add_send_count_field_to_invitation =
  Database.Migration.create_step
    ~label:"add resend count field to invitation"
    {sql|
      ALTER TABLE pool_invitations
        ADD COLUMN send_count integer NOT NULL DEFAULT 1 AFTER resent_at
    |sql}
;;

let update_send_count_field_of_invitation =
  Database.Migration.create_step
    ~label:"update resend count field of invitation"
    {sql|
      UPDATE pool_invitations
      SET send_count = send_count + 1
      WHERE send_count = 1 AND resent_at IS NOT NULL
    |sql}
;;

let add_invitation_reset_at_to_experiment =
  Database.Migration.create_step
    ~label:"add invitation reset at to\n   experiment"
    {sql| ALTER TABLE pool_experiments ADD COLUMN invitation_reset_at
   datetime NULL DEFAULT NULL AFTER text_message_session_reminder_lead_time
   |sql}
;;

let create_pool_mailing_invitation_mapping_table =
  Database.Migration.create_step
    ~label:"create pool mailing invitation mapping table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_mailing_invitations (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `mailing_uuid` binary(16) NOT NULL,
        `invitation_uuid` binary(16) NOT NULL,
        `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `mailing_uuid_invitation_uuid` (`mailing_uuid`, `invitation_uuid`),
      FOREIGN KEY (mailing_uuid) REFERENCES pool_mailing (`uuid`),
      FOREIGN KEY (invitation_uuid) REFERENCES pool_invitations (`uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309281435"
    |> add_step add_send_count_field_to_invitation
    |> add_step update_send_count_field_of_invitation
    |> add_step add_invitation_reset_at_to_experiment
    |> add_step create_pool_mailing_invitation_mapping_table)
;;
