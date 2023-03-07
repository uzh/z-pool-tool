open Entity

let encode_key_value value =
  (let open Value in
   match value with
   | DefaultReminderLeadTime v ->
     ( yojson_of_setting_key ReminderLeadTime
     , yojson_of_default_reminder_lead_time v )
   | TenantLanguages v ->
     yojson_of_setting_key Languages, yojson_of_tenant_languages v
   | TenantEmailSuffixes v ->
     yojson_of_setting_key EmailSuffixes, yojson_of_tenant_email_suffixes v
   | TenantContactEmail v ->
     yojson_of_setting_key ContactEmail, yojson_of_tenant_contact_email v
   | InactiveUserDisableAfter v ->
     ( yojson_of_setting_key InactiveUserDisableAfter
     , yojson_of_inactive_user_disable_after v )
   | InactiveUserWarning v ->
     ( yojson_of_setting_key InactiveUserWarning
     , yojson_of_inactive_user_warning v )
   | TermsAndConditions v ->
     yojson_of_setting_key TermsAndConditions, yojson_of_terms_and_conditions v
   | TriggerProfileUpdateAfter v ->
     ( yojson_of_setting_key TriggerProfileUpdateAfter
     , yojson_of_trigger_profile_update_after v ))
  |> fun (m, k) -> m |> Yojson.Safe.to_string, k |> Yojson.Safe.to_string
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
      | Languages -> value |> tenant_languages_of_yojson |> tenantlanguages
      | EmailSuffixes ->
        value |> tenant_email_suffixes_of_yojson |> tenantemailsuffixes
      | ContactEmail ->
        value |> tenant_contact_email_of_yojson |> tenantcontactemail
      | InactiveUserDisableAfter ->
        value
        |> inactive_user_disable_after_of_yojson
        |> inactiveuserdisableafter
      | InactiveUserWarning ->
        value |> inactive_user_warning_of_yojson |> inactiveuserwarning
      | TermsAndConditions ->
        value |> terms_and_conditions_of_yojson |> termsandconditions
      | TriggerProfileUpdateAfter ->
        value
        |> trigger_profile_update_after_of_yojson
        |> triggerprofileupdateafter
    in
    Ok { value; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup4
         string
         string
         Pool_common.Repo.CreatedAt.t
         Pool_common.Repo.UpdatedAt.t))
;;

module Write = struct
  include Write

  let t =
    let encode m =
      let key, value = encode_key_value m.Write.value in
      Ok (value, key)
    in
    let decode _ =
      failwith
        Pool_common.(
          Message.WriteOnlyModel |> Utils.error_to_string Language.En)
    in
    Caqti_type.(custom ~encode ~decode (tup2 string string))
  ;;
end
