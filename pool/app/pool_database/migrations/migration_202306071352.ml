let add_missing_columns_to_contacts =
  Database.Migration.Step.create
    ~label:"add missing columns to experiments"
    {sql|
    ALTER TABLE pool_contacts
      ADD COLUMN secondary_email varchar(255) AFTER email_verified,
      ADD COLUMN secondary_email_verified timestamp NULL AFTER secondary_email,
      ADD COLUMN sms_deactivated tinyint AFTER phone_number,
      ADD COLUMN admin_notes TEXT DEFAULT NULL AFTER experiment_type_preference_version,
      ADD COLUMN smtp_bounces_count SMALLINT(3) DEFAULT 0 AFTER profile_update_triggered_at,
      ADD COLUMN sign_in_count INTEGER DEFAULT 0 AFTER experiment_type_preference_version,
      ADD COLUMN last_sign_in_at timestamp NULL AFTER sign_in_count
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202306071352" |> add_step add_missing_columns_to_contacts)
;;
