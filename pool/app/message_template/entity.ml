open Ppx_yojson_conv_lib.Yojson_conv
module Field = Pool_message.Field

let model = Pool_message.Field.MessageTemplate

include Changelog.DefaultSettings

module Id = struct
  include Pool_common.Id
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
  ; label : Pool_common.MessageTemplateLabel.t
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

let to_human_label m = m.label |> Pool_common.MessageTemplateLabel.to_human

let prefixed_template_url ?append m =
  let base =
    Format.asprintf
      "%s/%s"
      (Pool_common.MessageTemplateLabel.prefixed_human_url m.label)
      (Id.value m.id)
  in
  append |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
;;

let template_hint label =
  let open Pool_common.MessageTemplateLabel in
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
  | InactiveContactWarning -> MessageTemplateInactiveContactWarning
  | InactiveContactDeactivation -> MessageTemplateInactiveContactWarning
  | ManualSessionMessage -> MessageTemplateManualSessionMessage
  | MatcherNotification -> MessageTemplateMatcherNotification
  | MatchFilterUpdateNotification -> MessageTemplateMatchFilterUpdateNotification
  | Login2FAToken -> MessageTemplateLogin2FAToken
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
