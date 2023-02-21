module Common = Pool_common
module PoolError = Common.Message

module Key = struct
  type t =
    | CreditsText
    | GreetingsText
    | PasswordPolicyText
    | WelcomeText
  [@@deriving eq, show, enum]

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "I18n Keys: Could not create list of all keys!"
  ;;

  let to_string = function
    | CreditsText -> "credits_text"
    | GreetingsText -> "greetings_text"
    | PasswordPolicyText -> "password_policy_text"
    | WelcomeText -> "welcome_text"
  ;;

  let of_string = function
    | "credits_text" -> Ok CreditsText
    | "greetings_text" -> Ok GreetingsText
    | "password_policy_text" -> Ok PasswordPolicyText
    | "welcome_text" -> Ok WelcomeText
    | _ -> Error PoolError.(Invalid Field.Key)
  ;;

  let is_rich_text = function
    | CreditsText | GreetingsText | WelcomeText -> true
    | PasswordPolicyText -> false
  ;;

  let schema () =
    Common.Utils.schema_decoder of_string to_string PoolError.Field.Key
  ;;
end

module Content = struct
  include Pool_common.Model.String

  let field = Common.Message.Field.Translation
  let schema () = schema field ()
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
