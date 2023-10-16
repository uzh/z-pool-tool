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

let add_send_count_field_to_invitation =
  Sihl.Database.Migration.create_step
    ~label:"add resend count field to invitation"
    {sql|
      ALTER TABLE pool_invitations
        ADD COLUMN send_count integer NOT NULL DEFAULT 1 AFTER resent_at
    |sql}
;;

let update_send_count_field_of_invitation =
  Sihl.Database.Migration.create_step
    ~label:"update resend count field of invitation"
    {sql|
      UPDATE pool_invitations
      SET send_count = send_count + 1
      WHERE send_count = 1 AND resent_at IS NOT NULL
    |sql}
;;

let add_invitation_reset_at_to_experiment =
  Sihl.Database.Migration.create_step
    ~label:"add invitation reset at to\n   experiment"
    {sql| ALTER TABLE pool_experiments ADD COLUMN invitation_reset_at
   datetime NULL DEFAULT NULL AFTER text_message_session_reminder_lead_time
   |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309281435"
    |> add_step add_optional_mailing_uuid_field_to_invitation
    |> add_step add_send_count_field_to_invitation
    |> add_step update_send_count_field_of_invitation
    |> add_step add_invitation_reset_at_to_experiment)
;;
