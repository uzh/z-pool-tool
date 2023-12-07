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
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | LanguagesUpdated languages ->
    let%lwt () = Repo.update pool (Value.TenantLanguages languages) in
    Lwt.return_unit
  | EmailSuffixesUpdated email_suffixes ->
    let%lwt () = Repo.update pool (Value.TenantEmailSuffixes email_suffixes) in
    Lwt.return_unit
  | DefaultReminderLeadTimeUpdated lead_time ->
    let%lwt () = Repo.update pool (Value.DefaultReminderLeadTime lead_time) in
    Lwt.return_unit
  | DefaultTextMsgReminderLeadTimeUpdated lead_time ->
    let%lwt () =
      Repo.update pool (Value.DefaultTextMsgReminderLeadTime lead_time)
    in
    Lwt.return_unit
  | ContactEmailUpdated contact_email ->
    let%lwt () = Repo.update pool (Value.TenantContactEmail contact_email) in
    Lwt.return_unit
  | InactiveUserDisableAfterUpdated inactive_user_disable_after ->
    let%lwt () =
      Repo.update
        pool
        (Value.InactiveUserDisableAfter inactive_user_disable_after)
    in
    Lwt.return_unit
  | InactiveUserWarningUpdated inactive_user_warning ->
    let%lwt () =
      Repo.update pool (Value.InactiveUserWarning inactive_user_warning)
    in
    Lwt.return_unit
  | TriggerProfileUpdateAfterUpdated trigger_update_after ->
    let%lwt () =
      Repo.update pool (Value.TriggerProfileUpdateAfter trigger_update_after)
    in
    Lwt.return_unit
;;
