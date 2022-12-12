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
    | SessionCancellationSubject
    | SessionCancellationText
    | SessionFinishSubject
    | SessionFinishText
    | TriggerProfileUpdateSubject
    | TriggerProfileUpdateText
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
    | SessionCancellationSubject -> "session_cancellation_subject"
    | SessionCancellationText -> "session_cancellation_text"
    | SessionFinishSubject -> "session_finish_subject"
    | SessionFinishText -> "session_finish_text"
    | TriggerProfileUpdateSubject -> "trigger_profile_update_subject"
    | TriggerProfileUpdateText -> "trigger_profile_update_text"
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
    | "session_cancellation_subject" -> Ok SessionCancellationSubject
    | "session_cancellation_text" -> Ok SessionCancellationText
    | "session_finish_subject" -> Ok SessionFinishSubject
    | "session_finish_text" -> Ok SessionFinishText
    | "trigger_profile_update_subject" -> Ok TriggerProfileUpdateSubject
    | "trigger_profile_update_text" -> Ok TriggerProfileUpdateText
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
    | SessionCancellationText
    | SessionFinishText
    | TriggerProfileUpdateText
    | WelcomeText -> true
    | ConfirmationSubject
    | ConfirmationWithoutSelfRegistrationSubject
    | ExperimentFinishSubject
    | InvitationSubject
    | InvitationWithoutSelfRegistrationSubject
    | ImportInvitationSubject
    | ReminderSubject
    | RescheduleSessionSubject
    | SessionCancellationSubject
    | SessionFinishSubject
    | TriggerProfileUpdateSubject -> false
  ;;

  let schema () =
    Common.Utils.schema_decoder of_string to_string PoolError.Field.Key
  ;;
end

module Content = struct
  include Pool_common.Model.String

  let field = Common.Message.Field.Translation
  let schema = schema ?validation:None field
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
