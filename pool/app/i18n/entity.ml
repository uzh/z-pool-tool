module Common = Pool_common
module PoolError = Common.Message

module Key = struct
  type t =
    | ConfirmationSubject
    | ConfirmationText
    | ConfirmationWithoutSelfRegistrationSubject
    | ConfirmationWithoutSelfRegistrationText
    | CreditsText
    | ExperimentFinishSubject
    | ExperimentFinishText
    | GreetingsText
    | ImportInvitationSubject
    | ImportInvitationText
    | InvitationSubject
    | InvitationText
    | InvitationWithoutSelfRegistrationSubject
    | InvitationWithoutSelfRegistrationText
    | ReminderSmsText
    | ReminderSubject
    | ReminderText
    | SessionFinishSubject
    | SessionFinishText
    | WelcomeText
    | PasswordPolicyText
  [@@deriving eq, show]

  let to_string = function
    | ConfirmationSubject -> "confirmation_subject"
    | ConfirmationText -> "confirmation_text"
    | ConfirmationWithoutSelfRegistrationSubject ->
      "confirmation_without_self_registration_subject"
    | ConfirmationWithoutSelfRegistrationText ->
      "confirmation_without_self_registration_text"
    | CreditsText -> "credits_text"
    | ExperimentFinishSubject -> "experiment_finish_subject"
    | ExperimentFinishText -> "experiment_finish_text"
    | GreetingsText -> "greetings_text"
    | ImportInvitationSubject -> "import_invitation_subject"
    | ImportInvitationText -> "import_invitation_text"
    | InvitationSubject -> "invitation_subject"
    | InvitationText -> "invitation_text"
    | InvitationWithoutSelfRegistrationSubject ->
      "invitation_without_self_registration_subject"
    | InvitationWithoutSelfRegistrationText ->
      "invitation_without_self_registration_text"
    | ReminderSmsText -> "reminder_sms_text"
    | ReminderSubject -> "reminder_subject"
    | ReminderText -> "reminder_text"
    | SessionFinishSubject -> "session_finish_subject"
    | SessionFinishText -> "session_finish_text"
    | WelcomeText -> "welcome_text"
    | PasswordPolicyText -> "password_policy_text"
  ;;

  let of_string = function
    | "confirmation_subject" -> Ok ConfirmationSubject
    | "confirmation_text" -> Ok ConfirmationText
    | "confirmation_without_self_registration_subject" ->
      Ok ConfirmationWithoutSelfRegistrationSubject
    | "confirmation_without_self_registration_text" ->
      Ok ConfirmationWithoutSelfRegistrationText
    | "credits_text" -> Ok ConfirmationText
    | "experiment_finish_subject" -> Ok ExperimentFinishSubject
    | "experiment_finish_text" -> Ok ExperimentFinishText
    | "greetings_text" -> Ok GreetingsText
    | "import_invitation_subject" -> Ok ImportInvitationSubject
    | "import_invitation_text" -> Ok ImportInvitationText
    | "invitation_subject" -> Ok InvitationSubject
    | "invitation_text" -> Ok InvitationText
    | "invitation_without_self_registration_subject" ->
      Ok InvitationWithoutSelfRegistrationSubject
    | "invitation_without_self_registration_text" ->
      Ok InvitationWithoutSelfRegistrationText
    | "reminder_sms_text" -> Ok ReminderSmsText
    | "reminder_subject" -> Ok ReminderSubject
    | "reminder_text" -> Ok ReminderText
    | "session_finish_subject" -> Ok SessionFinishSubject
    | "session_finish_text" -> Ok SessionFinishText
    | "welcome_text" -> Ok WelcomeText
    | "password_policy_text" -> Ok PasswordPolicyText
    | _ -> Error Pool_common.Message.(Invalid Key)
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder of_string to_string PoolError.Key
  ;;
end

module Content = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create content =
    if CCString.is_empty content
    then Error PoolError.(Invalid Translation)
    else Ok content
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.Translation
  ;;
end

type t =
  { id : Common.Id.t
  ; key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

let create key language content =
  { id = Common.Id.create (); key; language; content }
;;

let id m = m.id
let key m = m.key
let language m = m.language
let content m = m.content
let content_to_string m = m.content |> Content.value
