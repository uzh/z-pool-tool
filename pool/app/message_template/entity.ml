open Ppx_yojson_conv_lib.Yojson_conv
module Field = Pool_message.Field

let model = Pool_message.Field.MessageTemplate

include Changelog.DefaultSettings

module Id = struct
  include Pool_common.Id
end

module Label = struct
  let print = Utils.ppx_printer

  type t =
    | AccountSuspensionNotification [@name "account_suspension_notification"]
    [@printer print "account_suspension_notification"]
    | AssignmentCancellation [@name "assignment_cancellation"]
    [@printer print "assignment_cancellation"]
    | AssignmentConfirmation [@name "assignment_confirmation"]
    [@printer print "assignment_confirmation"]
    | AssignmentSessionChange [@name "assignment_session_change"]
    [@printer print "assignment_session_change"]
    | ContactEmailChangeAttempt [@name "contact_email_change_attempt"]
    [@printer print "contact_email_change_attempt"]
    | ContactRegistrationAttempt [@name "contact_registration_attempt"]
    [@printer print "contact_registration_attempt"]
    | EmailVerification [@name "email_verification"] [@printer print "email_verification"]
    | ExperimentInvitation [@name "experiment_invitation"]
    [@printer print "experiment_invitation"]
    | ManualSessionMessage [@name "manual_session_message"]
    [@printer print "manual_session_message"]
    | MatcherNotification [@name "matcher_notification"]
    [@printer print "matcher_notification"]
    | MatchFilterUpdateNotification [@name "match_filter_update_notification"]
    [@printer print "match_filter_update_notification"]
    | PasswordChange [@name "password_change"] [@printer print "password_change"]
    | PasswordReset [@name "password_reset"] [@printer print "password_reset"]
    | PhoneVerification [@name "phone_verification"] [@printer print "phone_verification"]
    | ProfileUpdateTrigger [@name "profile_update_trigger"]
    [@printer print "profile_update_trigger"]
    | SignUpVerification [@name "signup_verification"]
    [@printer print "signup_verification"]
    | SessionCancellation [@name "session_cancellation"]
    [@printer print "session_cancellation"]
    | SessionReminder [@name "session_reminder"] [@printer print "session_reminder"]
    | SessionReschedule [@name "session_reschedule"] [@printer print "session_reschedule"]
    | UserImport [@name "user_import"] [@printer print "user_import"]
    | WaitingListConfirmation [@name "waiting_list_confirmation"]
    [@printer print "waiting_list_confirmation"]
  [@@deriving eq, show { with_path = false }, yojson, variants]

  let read = Utils.Json.read_variant t_of_yojson
  let read_from_url m = m |> CCString.replace ~which:`All ~sub:"-" ~by:"_" |> read

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_message.(Error.Invalid Field.Label)
  ;;

  let to_human m =
    m |> show |> CCString.replace ~sub:"_" ~by:" " |> CCString.capitalize_ascii
  ;;

  let human_url m = m |> show |> CCString.replace ~sub:"_" ~by:"-"

  let prefixed_human_url m =
    m
    |> human_url
    |> Format.asprintf "%s/%s" Pool_message.Field.(human_url MessageTemplate)
  ;;

  let customizable_by_experiment =
    [ ExperimentInvitation
    ; SessionReminder
    ; AssignmentConfirmation
    ; WaitingListConfirmation
    ]
  ;;
end

module EmailSubject = struct
  include Pool_model.Base.String

  let field = Field.EmailSubject
  let schema () = schema field ()
end

module EmailText = struct
  include Pool_model.Base.String

  let field = Field.EmailText
  let schema () = schema field ()
end

module PlainText = struct
  include Pool_model.Base.String

  let field = Field.PlainText
  let schema () = schema field ()
end

module SmsText = struct
  include Pool_model.Base.String

  let field = Field.SmsText
  let schema () = schema field ()
end

module FallbackToEmail = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.FallbackToEmail
end

type t =
  { id : Id.t
  ; label : Label.t
  ; entity_uuid : Pool_common.Id.t option
  ; language : Pool_common.Language.t
  ; email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; plain_text : PlainText.t
  ; sms_text : SmsText.t
  }
[@@deriving eq, show, yojson]

module ManualMessage = struct
  type t =
    { recipient : Pool_user.EmailAddress.t
    ; language : Pool_common.Language.t
    ; email_subject : EmailSubject.t
    ; email_text : EmailText.t
    ; plain_text : PlainText.t
    }
  [@@deriving eq, show]
end

type layout =
  | Tenant of Pool_tenant.t
  | Root

let to_human_label m = m.label |> Label.to_human

let prefixed_template_url ?append m =
  let base = Format.asprintf "%s/%s" (Label.prefixed_human_url m.label) (Id.value m.id) in
  append |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
;;

let template_hint label =
  let open Label in
  let open Pool_common.I18n in
  match label with
  | AccountSuspensionNotification -> MessageTemplateAccountSuspensionNotification
  | AssignmentCancellation -> MessageTemplateAssignmentCancellation
  | AssignmentConfirmation -> MessageTemplateAssignmentConfirmation
  | AssignmentSessionChange -> MessageTemplateAssignmentSessionChange
  | ContactEmailChangeAttempt -> MessageTemplateContactEmailChangeAttempt
  | ContactRegistrationAttempt -> MessageTemplateContactRegistrationAttempt
  | EmailVerification -> MessageTemplateEmailVerification
  | ExperimentInvitation -> MessageTemplateExperimentInvitation
  | ManualSessionMessage -> MessageTemplateManualSessionMessage
  | MatcherNotification -> MessageTemplateMatcherNotification
  | MatchFilterUpdateNotification -> MessageTemplateMatchFilterUpdateNotification
  | PasswordChange -> MessageTemplatePasswordChange
  | PasswordReset -> MessageTemplatePasswordReset
  | PhoneVerification -> MessageTemplatePhoneVerification
  | ProfileUpdateTrigger -> MessageTemplateProfileUpdateTrigger
  | SignUpVerification -> MessageTemplateSignupVerification
  | SessionCancellation -> MessageTemplateSessionCancellation
  | SessionReminder -> MessageTemplateSessionReminder
  | SessionReschedule -> MessageTemplateSessionReschedule
  | UserImport -> MessageTemplateUserImport
  | WaitingListConfirmation -> MessageTemplateWaitingListConfirmation
;;
