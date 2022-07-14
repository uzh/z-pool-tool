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
    | PasswordPolicyText
    | ReminderSubject
    | ReminderSmsText
    | ReminderText
    | RescheduleSessionSubject
    | RescheduleSessionText
    | SessionFinishSubject
    | SessionFinishText
    | WelcomeText
  [@@deriving eq, show, enum]

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "I18n Keys: Could not create list of all keys!"
  ;;

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
    | PasswordPolicyText -> "password_policy_text"
    | ReminderSubject -> "reminder_subject"
    | ReminderSmsText -> "reminder_sms_text"
    | ReminderText -> "reminder_text"
    | RescheduleSessionSubject -> "reschedule_session_subject"
    | RescheduleSessionText -> "reschedule_session_text"
    | SessionFinishSubject -> "session_finish_subject"
    | SessionFinishText -> "session_finish_text"
    | WelcomeText -> "welcome_text"
  ;;

  let of_string = function
    | "confirmation_subject" -> Ok ConfirmationSubject
    | "confirmation_text" -> Ok ConfirmationText
    | "confirmation_without_self_registration_subject" ->
      Ok ConfirmationWithoutSelfRegistrationSubject
    | "confirmation_without_self_registration_text" ->
      Ok ConfirmationWithoutSelfRegistrationText
    | "credits_text" -> Ok CreditsText
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
    | "password_policy_text" -> Ok PasswordPolicyText
    | "reminder_subject" -> Ok ReminderSubject
    | "reminder_sms_text" -> Ok ReminderSmsText
    | "reminder_text" -> Ok ReminderText
    | "reschedule_session_subject" -> Ok RescheduleSessionSubject
    | "reschedule_session_text" -> Ok RescheduleSessionText
    | "session_finish_subject" -> Ok SessionFinishSubject
    | "session_finish_text" -> Ok SessionFinishText
    | "welcome_text" -> Ok WelcomeText
    | _ -> Error PoolError.(Invalid Field.Key)
  ;;

  let is_textarea = function
    | ConfirmationText
    | ConfirmationWithoutSelfRegistrationText
    | CreditsText
    | ExperimentFinishText
    | GreetingsText
    | ImportInvitationText
    | InvitationText
    | InvitationWithoutSelfRegistrationText
    | PasswordPolicyText
    | ReminderSmsText
    | ReminderText
    | RescheduleSessionText
    | SessionFinishText
    | WelcomeText -> true
    | ConfirmationSubject
    | ConfirmationWithoutSelfRegistrationSubject
    | ExperimentFinishSubject
    | InvitationSubject
    | InvitationWithoutSelfRegistrationSubject
    | ImportInvitationSubject
    | ReminderSubject
    | RescheduleSessionSubject
    | SessionFinishSubject -> false
  ;;

  let schema () =
    Common.Utils.schema_decoder of_string to_string PoolError.Field.Key
  ;;
end

module Content = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create content =
    if CCString.is_empty content
    then Error PoolError.(Invalid Field.Translation)
    else Ok content
  ;;

  let schema () =
    Common.Utils.schema_decoder create value PoolError.Field.Translation
  ;;
end

type t =
  { id : Common.Id.t
  ; key : Key.t
  ; language : Common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

let create key language content =
  { id = Common.Id.create (); key; language; content }
;;

let compare (one : t) (two : t) = CCString.compare (one |> show) (two |> show)
let id m = m.id
let key m = m.key
let language m = m.language
let content m = m.content
let content_to_string m = m.content |> Content.value
