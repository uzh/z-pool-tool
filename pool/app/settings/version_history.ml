open Entity

let key_of_setting =
  let open Value in
  function
  | DefaultReminderLeadTime _ -> ReminderLeadTime
  | DefaultTextMsgReminderLeadTime _ -> TextMsgReminderLeadTime
  | TenantLanguages _ -> Languages
  | TenantEmailSuffixes _ -> EmailSuffixes
  | TenantContactEmail _ -> ContactEmail
  | InactiveUserDisableAfter _ -> InactiveUserDisableAfter
  | InactiveUserWarning _ -> InactiveUserWarning
  | TriggerProfileUpdateAfter _ -> TriggerProfileUpdateAfter
  | UserImportFirstReminder _ -> UserImportFirstReminderAfter
  | UserImportSecondReminder _ -> UserImportSecondReminderAfter
;;

module Record = struct
  include Changelog.DefaultSettings
  include Value

  let model = Pool_message.Field.Setting
end

include Changelog.T (Record)
