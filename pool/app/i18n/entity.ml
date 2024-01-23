module Common = Pool_common
module PoolError = Common.Message

let print = Utils.ppx_printer

module Key = struct
  module Core = struct
    let field = Pool_common.Message.Field.Key

    type t =
      | AssistantRoleHint [@name "assistant_role_hint"]
      [@printer print "assistant_role_hint"]
      | CreditsText [@name "credits_text"] [@printer print "credits_text"]
      | ExperimenterRoleHint [@name "experimenter_role_hint"]
      [@printer print "experimenter_role_hint"]
      | GreetingsText [@name "greetings_text"] [@printer print "greetings_text"]
      | PasswordPolicyText [@name "password_policy_text"]
      [@printer print "password_policy_text"]
      | PrivacyPolicy [@name "privacy_policy"] [@printer print "privacy_policy"]
      | SignUpCTA [@name "signupcta"] [@printer print "signupcta"]
      | TermsAndConditions [@name "terms_and_conditions"]
      [@printer print "terms_and_conditions"]
      | WelcomeText [@name "welcome_text"] [@printer print "welcome_text"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let is_rich_text = function
    | AssistantRoleHint
    | CreditsText
    | ExperimenterRoleHint
    | GreetingsText
    | PrivacyPolicy
    | SignUpCTA
    | TermsAndConditions
    | WelcomeText -> true
    | PasswordPolicyText -> false
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
