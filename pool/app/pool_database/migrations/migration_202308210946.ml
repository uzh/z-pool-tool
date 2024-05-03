let add_unique_settings_key_constraint =
  Database.Migration.Step.create
    ~label:"add unique settings key constraint"
    {sql|
      ALTER TABLE pool_system_settings
        ADD CONSTRAINT unique_settings_key UNIQUE (settings_key)
    |sql}
;;

let rename_experiment_session_reminder_lead_time =
  Database.Migration.Step.create
    ~label:"rename experiment session reminder lead time"
    {sql|
      ALTER TABLE pool_experiments
        RENAME COLUMN session_reminder_lead_time TO email_session_reminder_lead_time
    |sql}
;;

let rename_session_reminder_lead_time =
  Database.Migration.Step.create
    ~label:"rename session reminder lead time"
    {sql|
      ALTER TABLE pool_sessions
        RENAME COLUMN reminder_lead_time TO email_reminder_lead_time,
        RENAME COLUMN reminder_sent_at TO email_reminder_sent_at
    |sql}
;;

let add_experimen_text_msg_reminder_lead_time =
  Database.Migration.Step.create
    ~label:"add experimen text msg reminder lead time"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN text_message_session_reminder_lead_time INTEGER AFTER email_session_reminder_lead_time
    |sql}
;;

let add_session_text_msg_reminder_lead_time =
  Database.Migration.Step.create
    ~label:"add session text msg reminder lead time"
    {sql|
      ALTER TABLE pool_sessions
        ADD COLUMN text_message_reminder_lead_time INTEGER AFTER email_reminder_sent_at,
        ADD COLUMN text_message_reminder_sent_at timestamp NULL DEFAULT NULL AFTER text_message_reminder_lead_time
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202308210946"
    |> add_step add_unique_settings_key_constraint
    |> add_step rename_experiment_session_reminder_lead_time
    |> add_step rename_session_reminder_lead_time
    |> add_step add_experimen_text_msg_reminder_lead_time
    |> add_step add_session_text_msg_reminder_lead_time)
;;
