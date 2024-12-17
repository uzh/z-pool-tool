open Entity

type event =
  | ContactEmailUpdated of ContactEmail.t
  | DefaultReminderLeadTimeUpdated of Pool_common.Reminder.EmailLeadTime.t
  | DefaultTextMsgReminderLeadTimeUpdated of Pool_common.Reminder.TextMessageLeadTime.t
  | EmailSuffixesUpdated of EmailSuffixes.t
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | LanguagesUpdated of Pool_common.Language.t list
  | TriggerProfileUpdateAfterUpdated of TriggerProfileUpdateAfter.t
  | UserImportFirstReminderAfterUpdated of UserImportReminder.FirstReminderAfter.t
  | UserImportSecondReminderAfterUpdated of UserImportReminder.SecondReminderAfter.t
  | PageScriptUpdated of (PageScript.t option * PageScript.location)
[@@deriving eq, show]

let[@warning "-27"] handle_event ?user_uuid pool : event -> unit Lwt.t
  =
  (* let open Utils.Lwt_result.Infix in *)
  (* let create_changelog key after =
    let open Version_history in
    let%lwt before = Repo.find_by_key pool key ||> fun { value; _ } -> value in
    let%lwt entity_uuid = Repo.find_setting_id pool key in
    insert pool ?user_uuid ~entity_uuid ~before ~after ()
  in
  let handle_update key setting =
    let%lwt () = create_changelog key setting in
    Repo.update pool setting
  in *)
  function
  | LanguagesUpdated languages -> Repo.TenantLanguages.update pool languages
  | EmailSuffixesUpdated suffixes -> Repo.TenantEmailSuffixes.update pool suffixes
  | DefaultReminderLeadTimeUpdated lead_time ->
    Repo.DefaultReminderLeadTime.update pool lead_time
  | DefaultTextMsgReminderLeadTimeUpdated lead_time ->
    Repo.DefaultTextMsgReminderLeadTime.update pool lead_time
  | ContactEmailUpdated contact_email -> Repo.TenantContactEmail.update pool contact_email
  | InactiveUserDisableAfterUpdated disable_after ->
    Repo.InactiveUserDisableAfter.update pool disable_after
  | InactiveUserWarningUpdated warning_after ->
    Repo.InactiveUserWarning.update pool warning_after
  | TriggerProfileUpdateAfterUpdated trigger_after ->
    Repo.TriggerProfileUpdateAfter.update pool trigger_after
  | UserImportFirstReminderAfterUpdated first_reminder_after ->
    Repo.UserImportFirstReminder.update pool first_reminder_after
  | UserImportSecondReminderAfterUpdated second_reminder_after ->
    Repo.UserImportSecondReminder.update pool second_reminder_after
  | PageScriptUpdated script -> Repo.PageScripts.update pool script
;;
