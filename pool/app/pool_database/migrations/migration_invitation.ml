let create_invitation_table =
  Database.Migration.Step.create
    ~label:"create invitation table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_invitations (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `experiment_id` bigint(20) NOT NULL,
        `participant_id` bigint(20) NOT NULL,
        `resent_at` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let rename_participant_to_subject =
  Database.Migration.Step.create
    ~label:"rename participant id to subject id"
    {sql|
      ALTER TABLE pool_invitations
        RENAME COLUMN participant_id TO subject_id
    |sql}
;;

let rename_subject_to_contact =
  Database.Migration.Step.create
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_invitations
        RENAME COLUMN subject_id TO contact_id
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "invitation"
    |> add_step create_invitation_table
    |> add_step rename_participant_to_subject
    |> add_step rename_subject_to_contact)
;;
