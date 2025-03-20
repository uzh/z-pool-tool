include Entity
include Event
module Guard = Entity_guard

let find_languages = Repo.TenantLanguages.find
let find_email_suffixes = Repo.TenantEmailSuffixes.find
let find_contact_email = Repo.TenantContactEmail.find
let find_inactive_user_disable_after = Repo.InactiveUserDisableAfter.find
let find_inactive_user_warning = Repo.InactiveUserWarning.find
let find_inactive_user_service_disabled = Repo.InactiveUserServiceDisabled.find
let find_trigger_profile_update_after = Repo.TriggerProfileUpdateAfter.find
let find_default_reminder_lead_time = Repo.DefaultReminderLeadTime.find
let find_default_text_msg_reminder_lead_time = Repo.DefaultTextMsgReminderLeadTime.find
let find_user_import_first_reminder_after = Repo.UserImportFirstReminder.find
let find_user_import_second_reminder_after = Repo.UserImportSecondReminder.find
let id_by_key = Repo.id_by_key

let default_language_of_list languages =
  languages
  |> CCList.head_opt
  |> CCOption.to_result Pool_message.(Error.NotFound Field.DefaultLanguage)
  |> Pool_common.Utils.get_or_failwith
;;

let default_language pool =
  let open Utils.Lwt_result.Infix in
  find_languages pool ||> default_language_of_list
;;

module PageScript = struct
  include PageScript

  let find = Repo.PageScripts.find
  let find_id = Repo.PageScripts.find_id
  let clear_cache = Repo.PageScripts.Cache.clear
end
