open Entity

type event =
  | ContactEmailUpdated of ContactEmail.t
  | ContactEmailCreated of ContactEmail.t * Database.Label.t
  | DefaultReminderLeadTimeUpdated of Pool_common.Reminder.EmailLeadTime.t
  | DefaultTextMsgReminderLeadTimeUpdated of Pool_common.Reminder.TextMessageLeadTime.t
  | EmailSuffixesUpdated of EmailSuffixes.t
  | SystemEmailTemplatesUpdated of SystemEmailTemplates.t
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | InactiveUserServiceDisabled of InactiveUser.ServiceDisabled.t
  | LanguagesUpdated of Pool_common.Language.t list
  | TriggerProfileUpdateAfterUpdated of TriggerProfileUpdateAfter.t
  | UserImportFirstReminderAfterUpdated of UserImportReminder.FirstReminderAfter.t
  | UserImportSecondReminderAfterUpdated of UserImportReminder.SecondReminderAfter.t
  | PageScriptUpdated of (PageScript.t option * PageScript.location)
  | PhoneVerificationEnabledUpdated of PhoneVerification.t
  | ProfileOnlyUpdated of ProfileOnly.t
[@@deriving eq, show]

let handle_event ?user_uuid db_ctx : event -> unit Lwt.t = function
  | LanguagesUpdated languages -> Repo.TenantLanguages.update ?user_uuid db_ctx languages
  | EmailSuffixesUpdated suffixes ->
    Repo.TenantEmailSuffixes.update ?user_uuid db_ctx suffixes
  | SystemEmailTemplatesUpdated templates ->
    Repo.SystemEmailTemplates.update ?user_uuid db_ctx templates
  | DefaultReminderLeadTimeUpdated lead_time ->
    Repo.DefaultReminderLeadTime.update ?user_uuid db_ctx lead_time
  | DefaultTextMsgReminderLeadTimeUpdated lead_time ->
    Repo.DefaultTextMsgReminderLeadTime.update ?user_uuid db_ctx lead_time
  | ContactEmailUpdated contact_email ->
    Repo.TenantContactEmail.update ?user_uuid db_ctx contact_email
  (* Using the database label passed through the event, as this event has the root context *)
  | ContactEmailCreated (contact_email, tenant_db) ->
    (* XXX(reynir): can [tenant_db] be different from [db_ctx]? *)
    Repo.TenantContactEmail.update (Database.label_ctx tenant_db) contact_email
  | InactiveUserDisableAfterUpdated disable_after ->
    Repo.InactiveUserDisableAfter.update ?user_uuid db_ctx disable_after
  | InactiveUserWarningUpdated warning_after ->
    Repo.InactiveUserWarning.update ?user_uuid db_ctx warning_after
  | InactiveUserServiceDisabled disabled ->
    Repo.InactiveUserServiceDisabled.update ?user_uuid db_ctx disabled
  | TriggerProfileUpdateAfterUpdated trigger_after ->
    Repo.TriggerProfileUpdateAfter.update ?user_uuid db_ctx trigger_after
  | UserImportFirstReminderAfterUpdated first_reminder_after ->
    Repo.UserImportFirstReminder.update ?user_uuid db_ctx first_reminder_after
  | UserImportSecondReminderAfterUpdated second_reminder_after ->
    Repo.UserImportSecondReminder.update ?user_uuid db_ctx second_reminder_after
  | PageScriptUpdated script -> Repo.PageScripts.update ?user_uuid db_ctx script
  | PhoneVerificationEnabledUpdated enabled ->
    Repo.PhoneVerificationEnabled.update ?user_uuid db_ctx enabled
  | ProfileOnlyUpdated profile_only ->
    Repo.ProfileOnly.update ?user_uuid db_ctx profile_only
;;
