open Entity

type event =
  | ContactEmailUpdated of ContactEmail.t
  | DefaultReminderLeadTimeUpdated of Pool_common.Reminder.EmailLeadTime.t
  | DefaultTextMsgReminderLeadTimeUpdated of
      Pool_common.Reminder.TextMessageLeadTime.t
  | EmailSuffixesUpdated of EmailSuffix.t list
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | LanguagesUpdated of Pool_common.Language.t list
  | TriggerProfileUpdateAfterUpdated of TriggerProfileUpdateAfter.t
  | UserImportFirstReminderAfterUpdated of
      UserImportReminder.FirstReminderAfter.t
  | UserImportSecondReminderAfterUpdated of
      UserImportReminder.SecondReminderAfter.t
[@@deriving eq, show]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let create_changelog after =
    let open Version_history in
    let key = key_of_setting after in
    let%lwt before = Repo.find_by_key pool key ||> fun { value; _ } -> value in
    let%lwt entity_uuid = Repo.find_setting_id pool key in
    insert pool ?user_uuid ~entity_uuid ~before ~after ()
  in
  let handle_update setting =
    let%lwt () = create_changelog setting in
    Repo.update pool setting
  in
  function
  | LanguagesUpdated languages ->
    handle_update (Value.TenantLanguages languages)
  | EmailSuffixesUpdated email_suffixes ->
    handle_update (Value.TenantEmailSuffixes email_suffixes)
  | DefaultReminderLeadTimeUpdated lead_time ->
    handle_update (Value.DefaultReminderLeadTime lead_time)
  | DefaultTextMsgReminderLeadTimeUpdated lead_time ->
    handle_update (Value.DefaultTextMsgReminderLeadTime lead_time)
  | ContactEmailUpdated contact_email ->
    handle_update (Value.TenantContactEmail contact_email)
  | InactiveUserDisableAfterUpdated inactive_user_disable_after ->
    handle_update (Value.InactiveUserDisableAfter inactive_user_disable_after)
  | InactiveUserWarningUpdated inactive_user_warning ->
    handle_update (Value.InactiveUserWarning inactive_user_warning)
  | TriggerProfileUpdateAfterUpdated trigger_update_after ->
    handle_update (Value.TriggerProfileUpdateAfter trigger_update_after)
  | UserImportFirstReminderAfterUpdated first_reminder_after ->
    handle_update (Value.UserImportFirstReminder first_reminder_after)
  | UserImportSecondReminderAfterUpdated second_reminder_after ->
    handle_update (Value.UserImportSecondReminder second_reminder_after)
;;
