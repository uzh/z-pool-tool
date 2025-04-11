open Entity

type event =
  | ContactEmailUpdated of ContactEmail.t
  | ContactEmailCreated of ContactEmail.t * Database.Label.t
  | DefaultReminderLeadTimeUpdated of Pool_common.Reminder.EmailLeadTime.t
  | DefaultTextMsgReminderLeadTimeUpdated of Pool_common.Reminder.TextMessageLeadTime.t
  | EmailSuffixesUpdated of EmailSuffixes.t
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | InactiveUserServiceDisabled of InactiveUser.ServiceDisabled.t
  | LanguagesUpdated of Pool_common.Language.t list
  | TriggerProfileUpdateAfterUpdated of TriggerProfileUpdateAfter.t
  | UserImportFirstReminderAfterUpdated of UserImportReminder.FirstReminderAfter.t
  | UserImportSecondReminderAfterUpdated of UserImportReminder.SecondReminderAfter.t
  | PageScriptUpdated of (PageScript.t option * PageScript.location)
[@@deriving eq, show]

let handle_event ?user_uuid pool : event -> unit Lwt.t = function
  | LanguagesUpdated languages -> Repo.TenantLanguages.update ?user_uuid pool languages
  | EmailSuffixesUpdated suffixes ->
    Repo.TenantEmailSuffixes.update ?user_uuid pool suffixes
  | DefaultReminderLeadTimeUpdated lead_time ->
    Repo.DefaultReminderLeadTime.update ?user_uuid pool lead_time
  | DefaultTextMsgReminderLeadTimeUpdated lead_time ->
    Repo.DefaultTextMsgReminderLeadTime.update ?user_uuid pool lead_time
  | ContactEmailUpdated contact_email ->
    Repo.TenantContactEmail.update ?user_uuid pool contact_email
  (* Using the database label passed through the event, as this event has the root context *)
  | ContactEmailCreated (contact_email, tenant_db) ->
    Repo.insert_contact_email tenant_db contact_email
  | InactiveUserDisableAfterUpdated disable_after ->
    Repo.InactiveUserDisableAfter.update ?user_uuid pool disable_after
  | InactiveUserWarningUpdated warning_after ->
    Repo.InactiveUserWarning.update ?user_uuid pool warning_after
  | InactiveUserServiceDisabled disabled ->
    Repo.InactiveUserServiceDisabled.update ?user_uuid pool disabled
  | TriggerProfileUpdateAfterUpdated trigger_after ->
    Repo.TriggerProfileUpdateAfter.update ?user_uuid pool trigger_after
  | UserImportFirstReminderAfterUpdated first_reminder_after ->
    Repo.UserImportFirstReminder.update ?user_uuid pool first_reminder_after
  | UserImportSecondReminderAfterUpdated second_reminder_after ->
    Repo.UserImportSecondReminder.update ?user_uuid pool second_reminder_after
  | PageScriptUpdated script -> Repo.PageScripts.update ?user_uuid pool script
;;
