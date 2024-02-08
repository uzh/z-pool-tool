open Ppx_yojson_conv_lib.Yojson_conv
module Message = Pool_common.Message
module TimeUnit = Pool_common.Model.TimeUnit

type command =
  { time_value : int
  ; time_unit : TimeUnit.t
  }

let update_command time_value time_unit = { time_value; time_unit }

let update_duration_schema integer_schema field =
  Pool_common.Utils.PoolConformist.(
    make Field.[ integer_schema; TimeUnit.named_schema field () ] update_command)
;;

module ContactEmail = struct
  include Pool_common.Model.String

  let field = Message.Field.ContactEmail

  let create email =
    let open Mrmime in
    match Mailbox.of_string email with
    | Ok _ -> Ok email
    | Error _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let schema () = schema ~validation:create field ()
  let of_string m = m
end

module EmailSuffix = struct
  include Pool_common.Model.String

  let field = Message.Field.EmailSuffix
  (* TODO: email address validation *)

  let schema () = schema field ()
  let of_string m = m
end

module InactiveUser = struct
  module DisableAfter = struct
    module Core = struct
      let name = Pool_common.Message.Field.InactiveUserDisableAfter
    end

    include Pool_common.Model.Duration (Core)
  end

  module Warning = struct
    module Core = struct
      let name = Pool_common.Message.Field.InactiveUserWarning
    end

    include Pool_common.Model.Duration (Core)
  end
end

module TriggerProfileUpdateAfter = struct
  module Core = struct
    type t

    let name = Pool_common.Message.Field.TriggerProfileUpdateAfter
  end

  include Pool_common.Model.Duration (Core)
end

module TermsAndConditions = struct
  module Terms = struct
    include Pool_common.Model.String

    let field = Message.Field.TermsAndConditions
    (* TODO: email address validation *)

    let schema () = schema field ()
    let of_string m = m
  end

  type t = Pool_common.Language.t * Terms.t [@@deriving eq, show, yojson]

  let create language content =
    let open CCResult in
    let* language = Pool_common.Language.create language in
    let* content = Terms.create content in
    Ok (language, content)
  ;;

  let value m = m
end

module UserImportReminder = struct
  module FirstReminderAfter = struct
    module Core = struct
      type t

      let name = Pool_common.Message.Field.FirstReminder
    end

    include Pool_common.Model.Duration (Core)
  end

  module SecondReminderAfter = struct
    module Core = struct
      type t

      let name = Pool_common.Message.Field.SecondReminder
    end

    include Pool_common.Model.Duration (Core)
  end
end

module Value = struct
  type default_reminder_lead_time = Pool_common.Reminder.EmailLeadTime.t
  [@@deriving eq, show, yojson]

  type default_text_msg_reminder_lead_time =
    Pool_common.Reminder.TextMessageLeadTime.t
  [@@deriving eq, show, yojson]

  type tenant_languages = Pool_common.Language.t list
  [@@deriving eq, show, yojson]

  type tenant_email_suffixes = EmailSuffix.t list [@@deriving eq, show, yojson]
  type tenant_contact_email = ContactEmail.t [@@deriving eq, show, yojson]

  type inactive_user_disable_after = InactiveUser.DisableAfter.t
  [@@deriving eq, show, yojson]

  type inactive_user_warning = InactiveUser.DisableAfter.t
  [@@deriving eq, show, yojson]

  type trigger_profile_update_after = TriggerProfileUpdateAfter.t
  [@@deriving eq, show, yojson]

  type t =
    | DefaultReminderLeadTime of default_reminder_lead_time
    | DefaultTextMsgReminderLeadTime of default_text_msg_reminder_lead_time
    | TenantLanguages of tenant_languages
    | TenantEmailSuffixes of tenant_email_suffixes
    | TenantContactEmail of tenant_contact_email
    | InactiveUserDisableAfter of inactive_user_disable_after
    | InactiveUserWarning of inactive_user_warning
    | TriggerProfileUpdateAfter of trigger_profile_update_after
    | UserImportFirstReminder of UserImportReminder.FirstReminderAfter.t
    | UserImportSecondReminder of UserImportReminder.SecondReminderAfter.t
  [@@deriving eq, show, yojson, variants]
end

type setting_key =
  | ReminderLeadTime [@name "default_reminder_lead_time"]
  | TextMsgReminderLeadTime [@name "default_text_msg_reminder_lead_time"]
  | Languages [@name "languages"]
  | EmailSuffixes [@name "email_suffixes"]
  | ContactEmail [@name "contact_email"]
  | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
  | InactiveUserWarning [@name "inactive_user_warning"]
  | TriggerProfileUpdateAfter [@name "trigger_profile_update_after"]
  | UserImportFirstReminderAfter [@name "user_import_first_reminder_after"]
  | UserImportSecondReminderAfter [@name "user_import_second_reminder_after"]
[@@deriving eq, show, yojson]

type t =
  { value : Value.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

module Write = struct
  type t = { value : Value.t }
end

let action_of_param = function
  | "create_emailsuffix" -> Ok `CreateEmailSuffix
  | "delete_emailsuffix" -> Ok `DeleteEmailSuffix
  | "update_default_lead_time" -> Ok `UpdateDefaultLeadTime
  | "update_text_msg_default_lead_time" -> Ok `UpdateTextMsgDefaultLeadTime
  | "update_inactive_user_disable_after" -> Ok `UpdateInactiveUserDisableAfter
  | "update_inactive_user_warning" -> Ok `UpdateInactiveUserWarning
  | "update_contact_email" -> Ok `UpdateContactEmail
  | "update_emailsuffix" -> Ok `UpdateEmailSuffixes
  | "update_languages" -> Ok `UpdateLanguages
  | "update_trigger_profile_update_after" -> Ok `UpdateTriggerProfileUpdateAfter
  | "user_import_first_reminder_after" -> Ok `UserImportFirstReminderAfter
  | "user_import_second_reminder_after" -> Ok `UserImportSecondReminderAfter
  | _ -> Error Pool_common.Message.DecodeAction
;;

let stringify_action = function
  | `CreateEmailSuffix -> "create_emailsuffix"
  | `DeleteEmailSuffix -> "delete_emailsuffix"
  | `UpdateDefaultLeadTime -> "update_default_lead_time"
  | `UpdateTextMsgDefaultLeadTime -> "update_text_msg_default_lead_time"
  | `UpdateInactiveUserDisableAfter -> "update_inactive_user_disable_after"
  | `UpdateInactiveUserWarning -> "update_inactive_user_warning"
  | `UpdateContactEmail -> "update_contact_email"
  | `UpdateEmailSuffixes -> "update_emailsuffix"
  | `UpdateLanguages -> "update_languages"
  | `UpdateTriggerProfileUpdateAfter -> "update_trigger_profile_update_after"
  | `UserImportFirstReminderAfter -> "user_import_first_reminder_after"
  | `UserImportSecondReminderAfter -> "user_import_second_reminder_after"
;;

let default_email_session_reminder_lead_time_key_yojson =
  yojson_of_setting_key ReminderLeadTime
;;

let default_text_message_session_reminder_lead_time_key_yojson =
  yojson_of_setting_key TextMsgReminderLeadTime
;;

let trigger_profile_update_after_key_yojson =
  yojson_of_setting_key TriggerProfileUpdateAfter
;;
