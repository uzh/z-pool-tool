include Entity
include Event
module Guard = Entity_guard

let[@warning "-4"] find_languages pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_languages pool
  ||> fun { value; _ } ->
  match value with
  | Value.TenantLanguages value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.Language) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_email_suffixes pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_email_suffixes pool
  ||> fun { value; _ } ->
  match value with
  | Value.TenantEmailSuffixes value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.EmailSuffix) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_contact_email pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_contact_email pool
  ||> fun { value; _ } ->
  match value with
  | Value.TenantContactEmail value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.ContactEmail) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_inactive_user_disable_after pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_inactive_user_disable_after pool
  ||> fun { value; _ } ->
  match value with
  | Value.InactiveUserDisableAfter value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.InactiveUserDisableAfter)
    |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_inactive_user_warning pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_inactive_user_warning pool
  ||> fun { value; _ } ->
  match value with
  | Value.InactiveUserWarning value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.InactiveUserWarning) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_trigger_profile_update_after pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_trigger_profile_update_after pool
  ||> fun { value; _ } ->
  match value with
  | Value.TriggerProfileUpdateAfter value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.TriggerProfileUpdateAfter)
    |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_default_reminder_lead_time pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_default_reminder_lead_time pool
  ||> fun { value; _ } ->
  match value with
  | Value.DefaultReminderLeadTime value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.LeadTime) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_default_text_msg_reminder_lead_time pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_default_text_msg_reminder_lead_time pool
  ||> fun { value; _ } ->
  match value with
  | Value.DefaultTextMsgReminderLeadTime value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.TextMessageLeadTime) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_user_import_first_reminder_after pool =
  let open Utils.Lwt_result.Infix in
  Repo.Sql.find pool Repo.RepoEntity.t Entity.UserImportFirstReminderAfter
  ||> fun { value; _ } ->
  match value with
  | Value.UserImportFirstReminder value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.FirstReminder) |> Pool_common.Utils.failwith
;;

let[@warning "-4"] find_user_import_second_reminder_after pool =
  let open Utils.Lwt_result.Infix in
  Repo.Sql.find pool Repo.RepoEntity.t Entity.UserImportSecondReminderAfter
  ||> fun { value; _ } ->
  match value with
  | Value.UserImportSecondReminder value -> value
  | _ ->
    (* Due to Repo function, this state cannot be reached. *)
    Pool_message.(Error.Retrieve Field.SecondReminder) |> Pool_common.Utils.failwith
;;

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
end
