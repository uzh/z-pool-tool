module Common = Pool_common

let print = Utils.ppx_printer

module Key = struct
  module Core = struct
    let field = Pool_message.Field.Key

    type t =
      | ActorPermissionCreateHint [@name "actor_permission_create_hint"]
      [@printer print "actor_permission_create_hint"]
      | ActorPermissionHint [@name "actor_permission_hint"]
      [@printer print "actor_permission_hint"]
      | AssistantRoleHint [@name "assistant_role_hint"]
      [@printer print "assistant_role_hint"]
      | CreditsText [@name "credits_text"] [@printer print "credits_text"]
      | DashboardUpcomingSessions [@name "dashboard_upcoming_sessions"]
      [@printer print "dashboard_upcoming_sessions"]
      | DashboardOnlineStudies [@name "dashboard_online_studies"]
      [@printer print "dashboard_online_studies"]
      | DashboardExperimentRegistration
      [@name "dashboard_experiment_registration"]
      [@printer print "dashboard_experiment_registration"]
      | DashboardExperimentHistory [@name "dashboard_experiment_history"]
      [@printer print "dashboard_experiment_history"]
      | DashboardWaitinglist [@name "dashboard_waiting_list"]
      [@printer print "dashboard_waiting_list"]
      | ExperimentNavigationTitle [@name "experiment_navigation_title"]
      [@printer print "experiment_navigation_title"]
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

  include Pool_model.Base.SelectorType (Core)
  include Core

  let input_type = function
    | ActorPermissionCreateHint
    | ActorPermissionHint
    | AssistantRoleHint
    | CreditsText
    | ExperimenterRoleHint
    | GreetingsText
    | PrivacyPolicy
    | SignUpCTA
    | TermsAndConditions
    | WelcomeText -> `RichText
    | PasswordPolicyText -> `TextArea
    | DashboardUpcomingSessions
    | DashboardOnlineStudies
    | DashboardExperimentRegistration
    | DashboardExperimentHistory
    | ExperimentNavigationTitle
    | DashboardWaitinglist -> `TextInput
  ;;
end

module Content = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Translation
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
