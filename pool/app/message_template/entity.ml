module Field = Pool_common.Message.Field

module Id = struct
  include Pool_common.Id
end

module Label = struct
  let print = Utils.ppx_printer

  type t =
    | AccountSuspensionNotification [@name "account_suspension_notification"]
        [@printer print "account_suspension_notification"]
    | AssignmentConfirmation [@name "assignment_confirmation"]
        [@printer print "assignment_confirmation"]
    | ContactRegistrationAttempt [@name "contact_registration_attempt"]
        [@printer print "contact_registration_attempt"]
    | EmailVerification [@name "email_verification"]
        [@printer print "email_verification"]
    | ExperimentInvitation [@name "experiment_invitation"]
        [@printer print "experiment_invitation"]
    | PasswordChange [@name "password_change"]
        [@printer print "password_change"]
    | PasswordReset [@name "password_reset"] [@printer print "password_reset"]
    | PhoneVerification [@name "phone_verification"]
        [@printer print "phone_verification"]
    | ProfileUpdateTrigger [@name "profile_update_trigger"]
        [@printer print "profile_update_trigger"]
    | SignUpVerification [@name "signup_verification"]
        [@printer print "signup_verification"]
    | SessionCancellation [@name "session_cancellation"]
        [@printer print "session_cancellation"]
    | SessionReminder [@name "session_reminder"]
        [@printer print "session_reminder"]
    | SessionReschedule [@name "session_reschedule"]
        [@printer print "session_reschedule"]
    | UserImport [@name "user_import"] [@printer print "user_import"]
  [@@deriving eq, show { with_path = false }, yojson, variants]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_common.Message.(Invalid Field.Label)
  ;;

  let to_human m =
    m |> show |> CCString.replace ~sub:"_" ~by:" " |> CCString.capitalize_ascii
  ;;

  let human_url m = m |> show |> CCString.replace ~sub:"_" ~by:"-"

  let prefixed_human_url m =
    m
    |> human_url
    |> Format.asprintf
         "%s/%s"
         Pool_common.Message.Field.(human_url MessageTemplate)
  ;;
end

module EmailSubject = struct
  include Pool_common.Model.String

  let field = Field.EmailSubject
  let schema () = schema field ()
end

module EmailText = struct
  include Pool_common.Model.String

  let field = Field.EmailText
  let schema () = schema field ()
end

module PlainText = struct
  include Pool_common.Model.String

  let field = Field.PlainText
  let schema () = schema field ()
end

module SmsText = struct
  include Pool_common.Model.String

  let field = Field.SmsText
  let schema () = schema field ()
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
[@@deriving eq, show]

type layout =
  | Tenant of Pool_tenant.t
  | Root

let to_human_label m = m.label |> Label.to_human

let prefixed_template_url ?append m =
  let base =
    Format.asprintf "%s/%s" (Label.prefixed_human_url m.label) (Id.value m.id)
  in
  append |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
;;
