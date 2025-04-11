open Ppx_yojson_conv_lib.Yojson_conv
module TimeUnit = Pool_model.Base.TimeUnit

type command =
  { time_value : int
  ; time_unit : TimeUnit.t
  }

let update_command time_value time_unit = { time_value; time_unit }

let update_duration_schema integer_schema field =
  Pool_conformist.(
    make Field.[ integer_schema; TimeUnit.named_schema field () ] update_command)
;;

module TermsAndConditions = struct
  module Terms = struct
    include Pool_model.Base.String

    let field = Pool_message.Field.TermsAndConditions
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

module Key = struct
  let printer = Utils.ppx_printer

  type t =
    | ContactEmail [@name "contact_email"] [@printer printer "contact_email"]
    | EmailSuffixes [@name "email_suffixes"] [@printer printer "email_suffixes"]
    | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
    [@printer printer "inactive_user_disable_after"]
    | InactiveUserWarning [@name "inactive_user_warning"]
    [@printer printer "inactive_user_warning"]
    | InactiveUserServiceDisabled [@name "inactive_user_service_disabled"]
    [@printer printer "inactive_user_service_disabled"]
    | Languages [@name "languages"] [@printer printer "languages"]
    | ReminderLeadTime [@name "default_reminder_lead_time"]
    [@printer printer "default_reminder_lead_time"]
    | TextMsgReminderLeadTime [@name "default_text_msg_reminder_lead_time"]
    [@printer printer "default_text_msg_reminder_lead_time"]
    | TriggerProfileUpdateAfter [@name "trigger_profile_update_after"]
    [@printer printer "trigger_profile_update_after"]
    | UserImportFirstReminderAfter [@name "user_import_first_reminder_after"]
    [@printer printer "user_import_first_reminder_after"]
    | UserImportSecondReminderAfter [@name "user_import_second_reminder_after"]
    [@printer printer "user_import_second_reminder_after"]
  [@@deriving eq, show { with_path = false }, yojson]

  let read = Utils.Json.read_variant t_of_yojson
  let to_json_string key = key |> yojson_of_t |> Yojson.Safe.to_string
end

module ContactEmail = struct
  include Pool_model.Base.String

  let key = Key.ContactEmail
  let field = Pool_message.Field.ContactEmail

  let create email =
    let open Mrmime in
    match Mailbox.of_string email with
    | Ok _ -> Ok email
    | Error _ -> Error Pool_message.(Error.Invalid field)
  ;;

  let schema () = schema ~validation:create field ()
  let of_string m = m
end

module EmailSuffix = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.EmailSuffix
  (* TODO: email address validation *)

  let schema () = schema field ()
  let of_string m = m
end

module EmailSuffixes = struct
  type t = EmailSuffix.t list [@@deriving eq, show, yojson]

  let key = Key.EmailSuffixes
end

module EmailReminderLeadTime = struct
  type t = Pool_common.Reminder.EmailLeadTime.t [@@deriving eq, show, yojson]

  let key = Key.ReminderLeadTime
end

module TextMsgReminderLeadTime = struct
  type t = Pool_common.Reminder.TextMessageLeadTime.t [@@deriving eq, show, yojson]

  let key = Key.TextMsgReminderLeadTime
end

module InactiveUser = struct
  module DisableAfter = struct
    module Core = struct
      let name = Pool_message.Field.InactiveUserDisableAfter
    end

    include Pool_model.Base.Duration (Core)

    let key = Key.InactiveUserDisableAfter
  end

  module Warning = struct
    module TimeSpan = struct
      module Core = struct
        let name = Pool_message.Field.InactiveUserWarning
      end

      include Pool_model.Base.Duration (Core)
    end

    type t = TimeSpan.t list [@@deriving eq, show, yojson]

    let key = Key.InactiveUserWarning
  end

  module ServiceDisabled = struct
    let field = Pool_message.Field.InactiveUserDisableService

    include Pool_model.Base.Boolean

    let schema = schema ~default:false field
    let key = Key.InactiveUserServiceDisabled
  end
end

module TriggerProfileUpdateAfter = struct
  module Core = struct
    type t

    let name = Pool_message.Field.TriggerProfileUpdateAfter
  end

  include Pool_model.Base.Duration (Core)

  let key = Key.TriggerProfileUpdateAfter
end

module TenantLanguages = struct
  type t = Pool_common.Language.t list [@@deriving eq, show, yojson]

  let key = Key.Languages
end

module UserImportReminder = struct
  let validate m =
    let open Ptime.Span in
    let day = 60 * 60 * 24 |> of_int_s in
    if m >= day then Ok m else Error Pool_message.Error.TooShort
  ;;

  module FirstReminderAfter = struct
    module Core = struct
      type t

      let name = Pool_message.Field.FirstReminder
    end

    include Pool_model.Base.Duration (Core)

    let validate = validate
    let key = Key.UserImportFirstReminderAfter
  end

  module SecondReminderAfter = struct
    module Core = struct
      type t

      let name = Pool_message.Field.SecondReminder
    end

    include Pool_model.Base.Duration (Core)

    let validate = validate
    let key = Key.UserImportSecondReminderAfter
  end
end

module PageScript = struct
  include Changelog.DefaultSettings
  include Pool_model.Base.String

  let model = Pool_message.Field.Setting

  type location =
    | Head [@name "head"] [@printer Utils.ppx_printer "head"]
    | Body [@name "body"] [@printer Utils.ppx_printer "body"]
  [@@deriving eq, show { with_path = false }, yojson]

  let schema field () = schema field ()
  let of_string m = m
  let read_location = Utils.Json.read_variant location_of_yojson

  type page_scripts =
    { head : t option
    ; body : t option
    }
end

module PageScriptChangelog = Changelog.T (PageScript)

let action_of_param = function
  | "create_emailsuffix" -> Ok `CreateEmailSuffix
  | "delete_emailsuffix" -> Ok `DeleteEmailSuffix
  | "update_default_lead_time" -> Ok `UpdateDefaultLeadTime
  | "update_text_msg_default_lead_time" -> Ok `UpdateTextMsgDefaultLeadTime
  | "update_inactive_user_disable_after" -> Ok `UpdateInactiveUserDisableAfter
  | "update_inactive_user_warning" -> Ok `UpdateInactiveUserWarning
  | "disable_inactive_user_service" -> Ok `UpdateUnactiveUserServiceDisabled
  | "update_contact_email" -> Ok `UpdateContactEmail
  | "update_emailsuffix" -> Ok `UpdateEmailSuffixes
  | "update_languages" -> Ok `UpdateLanguages
  | "update_trigger_profile_update_after" -> Ok `UpdateTriggerProfileUpdateAfter
  | "user_import_first_reminder_after" -> Ok `UserImportFirstReminderAfter
  | "user_import_second_reminder_after" -> Ok `UserImportSecondReminderAfter
  | "update_head_scripts" -> Ok `UpdateHeadScripts
  | "update_body_scripts" -> Ok `UpdateBodyScripts
  | _ -> Error Pool_message.Error.DecodeAction
;;

let stringify_action = function
  | `CreateEmailSuffix -> "create_emailsuffix"
  | `DeleteEmailSuffix -> "delete_emailsuffix"
  | `UpdateDefaultLeadTime -> "update_default_lead_time"
  | `UpdateTextMsgDefaultLeadTime -> "update_text_msg_default_lead_time"
  | `UpdateInactiveUserDisableAfter -> "update_inactive_user_disable_after"
  | `UpdateInactiveUserWarning -> "update_inactive_user_warning"
  | `UpdateUnactiveUserServiceDisabled -> "disable_inactive_user_service"
  | `UpdateContactEmail -> "update_contact_email"
  | `UpdateEmailSuffixes -> "update_emailsuffix"
  | `UpdateLanguages -> "update_languages"
  | `UpdateTriggerProfileUpdateAfter -> "update_trigger_profile_update_after"
  | `UserImportFirstReminderAfter -> "user_import_first_reminder_after"
  | `UserImportSecondReminderAfter -> "user_import_second_reminder_after"
  | `UpdateHeadScripts -> "update_head_scripts"
  | `UpdateBodyScripts -> "update_body_scripts"
;;

let default_email_session_reminder_lead_time_key_yojson =
  Key.(yojson_of_t ReminderLeadTime)
;;

let default_text_message_session_reminder_lead_time_key_yojson =
  Key.(yojson_of_t TextMsgReminderLeadTime)
;;

let trigger_profile_update_after_key_yojson = Key.(yojson_of_t TriggerProfileUpdateAfter)
