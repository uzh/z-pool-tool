module Field = Pool_common.Message.Field

module Id = struct
  include Pool_common.Id
end

module Label = struct
  let go m fmt _ = Format.pp_print_string fmt m

  (** ConfirmationWithoutSelfRegistration

      - Your expression of interest to the experiment.
      - We have received your expression of interest to the experiment and \
        will contact you shortly.

      ImportInvitation

      - We have updated our management software for organizing experiments.

      InvitationWithoutSelfRegistrationSubject

      - We would like to invite you to an upcoming the experiment.

      ExperimentFinish

      - Please complete the participants data.

      SessionFinishSubject

      - Please complete the participants data. *)
  type t =
    | AssignmentConfirmation [@name "assignment_confirmation"]
        [@printer go "assignment_confirmation"]
    | EmailVerification [@name "email_verification"]
        [@printer go "email_verification"]
    | ExperimentInvitation [@name "experiment_invitation"]
        [@printer go "experiment_invitation"]
    | PasswordChange [@name "password_change"] [@printer go "password_change"]
    | PasswordReset [@name "password_reset"] [@printer go "password_reset"]
    | ProfileUpdateTrigger [@name "profile_update_trigger"]
        [@printer go "profile_update_trigger"]
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

let to_human_label m = m.label |> Label.to_human

let prefixed_template_url ?append m =
  let base =
    Format.asprintf "%s/%s" (Label.prefixed_human_url m.label) (Id.value m.id)
  in
  append |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
;;
