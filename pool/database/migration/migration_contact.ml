let create_participant_table =
  Sihl.Database.Migration.create_step
    ~label:"create participant table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_participants (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `user_uuid` binary(16) NOT NULL,
        `recruitment_channel` varchar(128) NOT NULL,
        `terms_accepted_at` timestamp NULL,
        `paused` boolean NOT NULL,
        `disabled` boolean NOT NULL,
        `verified` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`user_uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_field_versioning =
  Sihl.Database.Migration.create_step
    ~label:"add field versioning for participants"
    {sql|
     ALTER TABLE pool_participants
     ADD COLUMN firstname_version bigint(20) NOT NULL DEFAULT 0 AFTER verified,
     ADD COLUMN lastname_version bigint(20) NOT NULL DEFAULT 0 AFTER firstname_version,
     ADD COLUMN paused_version bigint(20) NOT NULL DEFAULT 0 AFTER lastname_version
    |sql}
;;

let add_user_language =
  Sihl.Database.Migration.create_step
    ~label:"add field versioning for participants"
    {sql|
     ALTER TABLE pool_participants
     ADD COLUMN language varchar(128) AFTER terms_accepted_at
    |sql}
;;

let add_user_language_version =
  Sihl.Database.Migration.create_step
    ~label:"add field versioning for participant language"
    {sql|
     ALTER TABLE pool_participants
     ADD COLUMN language_version bigint(20) NOT NULL DEFAULT 0 AFTER paused_version
    |sql}
;;

let add_user_email_verified_counts =
  Sihl.Database.Migration.create_step
    ~label:
      "add field for email verification, assignment inclusive show up count"
    {sql|
     ALTER TABLE pool_participants
     ADD COLUMN email_verified timestamp NULL AFTER verified,
     ADD COLUMN num_invitations SMALLINT(3) UNSIGNED NOT NULL AFTER email_verified,
     ADD COLUMN num_assignments SMALLINT(3) UNSIGNED NOT NULL AFTER num_invitations
    |sql}
;;

let rename_participants_to_subjects_table =
  Sihl.Database.Migration.create_step
    ~label:"rename participants to subjects table"
    {sql|
      ALTER TABLE pool_participants
        RENAME pool_subjects
    |sql}
;;

let rename_subjects_to_contacts_table =
  Sihl.Database.Migration.create_step
    ~label:"rename subjects to contacts table"
    {sql|
      ALTER TABLE pool_subjects
        RENAME pool_contacts
    |sql}
;;

let add_experiment_type_preference =
  Sihl.Database.Migration.create_step
    ~label:"add experiment_type_preference"
    {sql|
     ALTER TABLE pool_contacts
     ADD COLUMN experiment_type_preference varchar(128) DEFAULT NULL AFTER language,
     ADD COLUMN experiment_type_preference_version bigint(20) NOT NULL DEFAULT 0 AFTER language_version
    |sql}
;;

let add_profile_update_triggered_timestamp =
  Sihl.Database.Migration.create_step
    ~label:"add profile update triggered timestamp"
    {sql|
      ALTER TABLE pool_contacts
      ADD COLUMN profile_updated_at timestamp DEFAULT CURRENT_TIMESTAMP AFTER experiment_type_preference_version,
      ADD COLUMN profile_update_triggered_at timestamp NULL AFTER profile_updated_at
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "participant"
    |> add_step create_participant_table
    |> add_step add_field_versioning
    |> add_step add_user_language
    |> add_step add_user_language_version
    |> add_step add_user_email_verified_counts
    |> add_step rename_participants_to_subjects_table
    |> add_step rename_subjects_to_contacts_table
    |> add_step add_experiment_type_preference
    |> add_step add_profile_update_triggered_timestamp)
;;
