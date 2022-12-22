module Field = Pool_common.Message.Field

module Id = struct
  include Pool_common.Id
end

module Label = struct
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | AssignmentConfirmation [@name "assignment_confirmation"]
        [@printer go "assignment_confirmation"]
    | EmailVerification [@name "email_verification"]
        [@printer go "email_verification"]
    | ExperimentInvitation [@name "experiment_invitation"]
        [@printer go "experiment_invitation"]
    | PasswordChange [@name "password_change"] [@printer go "password_change"]
    | PasswordReset [@name "password_reset"] [@printer go "password_reset"]
    | SignUpVerification [@name "signup_verification"]
        [@printer go "signup_verification"]
    | SessionCancellation [@name "session_cancellation"]
        [@printer go "session_cancellation"]
    | SessionReminder [@name "session_reminder"]
        [@printer go "session_reminder"]
    | SessionReschedule [@name "session_reschedule"]
        [@printer go "session_reschedule"]
  [@@deriving eq, show { with_path = false }, yojson, variants]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_common.Message.(Invalid Field.Label)
  ;;
end

module EmailSubject = struct
  include Pool_common.Model.String

  let field = Field.EmailSubject
  let schema = schema ?validation:None field
end

module EmailText = struct
  include Pool_common.Model.String

  let field = Field.EmailText
  let schema = schema ?validation:None field
end

module SmsText = struct
  include Pool_common.Model.String

  let field = Field.SmsText
  let schema = schema ?validation:None field
end

type t =
  { id : Id.t
  ; label : Label.t
  ; entity_uuid : Pool_common.Id.t option
  ; language : Pool_common.Language.t
  ; email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; sms_text : SmsText.t
  }
[@@deriving eq, show]

type layout =
  | Tenant of Pool_tenant.t
  | Root
