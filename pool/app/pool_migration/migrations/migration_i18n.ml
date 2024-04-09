let create_i18n_table =
  Database.Migration.create_step
    ~label:"create i18n table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_i18n (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `i18n_key` varchar(128) NOT NULL,
        `language` varchar(128) NOT NULL,
        `content` text,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let remove_unused_rows =
  Database.Migration.create_step
    ~label:"remove unused rows from i18n table"
    {sql|
      DELETE FROM pool_i18n
      WHERE i18n_key IN ('confirmation_subject', 'confirmation_text', 'confirmation_without_self_registration_subject', 'confirmation_without_self_registration_text', 'experiment_finish_subject', 'experiment_finish_text', 'import_invitation_subject', 'import_invitation_text', 'invitation_subject', 'invitation_text', 'invitation_without_self_registration_subject', 'invitation_without_self_registration_text', 'reminder_subject', 'reminder_sms_text', 'reminder_text', 'reschedule_session_subject', 'reschedule_session_text', 'session_cancellation_subject', 'session_cancellation_text', 'session_finish_subject', 'session_finish_text', 'trigger_profile_update_subject', 'trigger_profile_update_text')
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "i18n" |> add_step create_i18n_table |> add_step remove_unused_rows)
;;
