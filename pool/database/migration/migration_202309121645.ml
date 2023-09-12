let add_default_text_message_lead_time =
  Sihl.Database.Migration.create_step
    ~label:"add default text message lead time"
    {sql|
      INSERT INTO pool_system_settings (uuid, settings_key, `value`)
      SELECT X'CA8E8C28FBAA495F86697FDE3EA730EB', '["default_text_msg_reminder_lead_time"]', 86400
      WHERE NOT EXISTS (
        SELECT 1
        FROM pool_system_settings
        WHERE settings_key = '["default_text_msg_reminder_lead_time"]'
      )
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309121645" |> add_step add_default_text_message_lead_time)
;;
