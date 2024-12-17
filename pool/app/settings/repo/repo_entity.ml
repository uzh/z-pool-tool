open Entity

let encode_key_value value =
  (let open Value in
   match value with
   | DefaultReminderLeadTime v -> ReminderLeadTime, yojson_of_default_reminder_lead_time v
   | DefaultTextMsgReminderLeadTime v ->
     TextMsgReminderLeadTime, yojson_of_default_text_msg_reminder_lead_time v
   | TenantLanguages v -> Languages, yojson_of_tenant_languages v
   | TenantEmailSuffixes v -> EmailSuffixes, yojson_of_tenant_email_suffixes v
   | TenantContactEmail v -> ContactEmail, yojson_of_tenant_contact_email v
   | InactiveUserDisableAfter v ->
     InactiveUserDisableAfter, yojson_of_inactive_user_disable_after v
   | InactiveUserWarning v -> InactiveUserWarning, yojson_of_inactive_user_warning v
   | TriggerProfileUpdateAfter v ->
     TriggerProfileUpdateAfter, yojson_of_trigger_profile_update_after v
   | UserImportFirstReminder v ->
     UserImportFirstReminderAfter, UserImportReminder.FirstReminderAfter.yojson_of_t v
   | UserImportSecondReminder v ->
     UserImportSecondReminderAfter, UserImportReminder.SecondReminderAfter.yojson_of_t v)
  |> fun (m, k) ->
  m |> yojson_of_setting_key |> Yojson.Safe.to_string, k |> Yojson.Safe.to_string
;;

let t =
  let encode m =
    let key, value = encode_key_value m.value in
    Ok (key, value, m.created_at, m.updated_at)
  in
  let decode (key, value, created_at, updated_at) =
    let key = key |> Yojson.Safe.from_string |> setting_key_of_yojson in
    let value =
      let open Value in
      let value = value |> Yojson.Safe.from_string in
      match key with
      | ReminderLeadTime ->
        value |> default_reminder_lead_time_of_yojson |> defaultreminderleadtime
      | TextMsgReminderLeadTime ->
        value
        |> default_text_msg_reminder_lead_time_of_yojson
        |> defaulttextmsgreminderleadtime
      | Languages -> value |> tenant_languages_of_yojson |> tenantlanguages
      | EmailSuffixes -> value |> tenant_email_suffixes_of_yojson |> tenantemailsuffixes
      | ContactEmail -> value |> tenant_contact_email_of_yojson |> tenantcontactemail
      | InactiveUserDisableAfter ->
        value |> inactive_user_disable_after_of_yojson |> inactiveuserdisableafter
      | InactiveUserWarning ->
        value |> inactive_user_warning_of_yojson |> inactiveuserwarning
      | TriggerProfileUpdateAfter ->
        value |> trigger_profile_update_after_of_yojson |> triggerprofileupdateafter
      | UserImportFirstReminderAfter ->
        value
        |> UserImportReminder.FirstReminderAfter.t_of_yojson
        |> userimportfirstreminder
      | UserImportSecondReminderAfter ->
        value
        |> UserImportReminder.SecondReminderAfter.t_of_yojson
        |> userimportsecondreminder
    in
    Ok { value; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t4 string string Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t))
;;

module Write = struct
  include Write

  let t =
    let encode m =
      let key, value = encode_key_value m.Write.value in
      Ok (value, key)
    in
    let decode _ = Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith in
    Caqti_type.(custom ~encode ~decode (t2 string string))
  ;;
end
