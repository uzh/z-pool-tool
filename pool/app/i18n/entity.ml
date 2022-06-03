module Common = Pool_common
module PoolError = Common.Message

module Key = struct
  type t =
    | ConfirmationContact
    | ConfirmationText
    | ConfirmationWithoutSelfRegistrationContact
    | ConfirmationWithoutSelfRegistrationText
    | CreditsText
    | ExperimentFinishContact
    | ExperimentFinishText
    | GreetingsText
    | ImportInvitationContact
    | ImportInvitationText
    | InvitationContact
    | InvitationText
    | InvitationWithoutSelfRegistrationContact
    | InvitationWithoutSelfRegistrationText
    | PasswordPolicyText
    | ReminderContact
    | ReminderSmsText
    | ReminderText
    | SessionFinishContact
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
    | ConfirmationContact -> "confirmation_contact"
    | ConfirmationText -> "confirmation_text"
    | ConfirmationWithoutSelfRegistrationContact ->
      "confirmation_without_self_registration_contact"
    | ConfirmationWithoutSelfRegistrationText ->
      "confirmation_without_self_registration_text"
    | CreditsText -> "credits_text"
    | ExperimentFinishContact -> "experiment_finish_contact"
    | ExperimentFinishText -> "experiment_finish_text"
    | GreetingsText -> "greetings_text"
    | ImportInvitationContact -> "import_invitation_contact"
    | ImportInvitationText -> "import_invitation_text"
    | InvitationContact -> "invitation_contact"
    | InvitationText -> "invitation_text"
    | InvitationWithoutSelfRegistrationContact ->
      "invitation_without_self_registration_contact"
    | InvitationWithoutSelfRegistrationText ->
      "invitation_without_self_registration_text"
    | PasswordPolicyText -> "password_policy_text"
    | ReminderContact -> "reminder_contact"
    | ReminderSmsText -> "reminder_sms_text"
    | ReminderText -> "reminder_text"
    | SessionFinishContact -> "session_finish_contact"
    | SessionFinishText -> "session_finish_text"
    | WelcomeText -> "welcome_text"
  ;;

  let of_string = function
    | "confirmation_contact" -> Ok ConfirmationContact
    | "confirmation_text" -> Ok ConfirmationText
    | "confirmation_without_self_registration_contact" ->
      Ok ConfirmationWithoutSelfRegistrationContact
    | "confirmation_without_self_registration_text" ->
      Ok ConfirmationWithoutSelfRegistrationText
    | "credits_text" -> Ok ConfirmationText
    | "experiment_finish_contact" -> Ok ExperimentFinishContact
    | "experiment_finish_text" -> Ok ExperimentFinishText
    | "greetings_text" -> Ok GreetingsText
    | "import_invitation_contact" -> Ok ImportInvitationContact
    | "import_invitation_text" -> Ok ImportInvitationText
    | "invitation_contact" -> Ok InvitationContact
    | "invitation_text" -> Ok InvitationText
    | "invitation_without_self_registration_contact" ->
      Ok InvitationWithoutSelfRegistrationContact
    | "invitation_without_self_registration_text" ->
      Ok InvitationWithoutSelfRegistrationText
    | "password_policy_text" -> Ok PasswordPolicyText
    | "reminder_contact" -> Ok ReminderContact
    | "reminder_sms_text" -> Ok ReminderSmsText
    | "reminder_text" -> Ok ReminderText
    | "session_finish_contact" -> Ok SessionFinishContact
    | "session_finish_text" -> Ok SessionFinishText
    | "welcome_text" -> Ok WelcomeText
    | _ -> Error PoolError.(Invalid Field.Key)
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
