open Entity

type create =
  { title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; experiment_type : ExperimentType.t option
  ; invitation_subject : InvitationTemplate.Subject.t option
  ; invitation_text : InvitationTemplate.Text.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; session_reminder_subject : Pool_common.Reminder.Subject.t option
  ; session_reminder_text : Pool_common.Reminder.Text.t option
  }
[@@deriving eq, show]

type event =
  | Created of t
  | Updated of t
  | Destroyed of Common.Id.t
  | ExperimenterAssigned of t * Admin.experimenter Admin.t
  | ExperimenterDivested of t * Admin.experimenter Admin.t
  | AssistantAssigned of t * Admin.assistant Admin.t
  | AssistantDivested of t * Admin.assistant Admin.t

let handle_event pool : event -> unit Lwt.t = function
  | Created t -> Repo.insert pool t
  | Updated t -> Repo.update pool t
  | Destroyed experiment_id -> Repo.destroy pool experiment_id
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    Permission.divest (Admin.user user) (Role.operator experiment.id)
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    -> Permission.divest (Admin.user user) (Role.operator experiment.id)
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | Created experiment_one, Created experiment_two
  | Updated experiment_one, Updated experiment_two ->
    equal experiment_one experiment_two
  | Destroyed one, Destroyed two -> Id.equal one two
  | ( ExperimenterAssigned (experiment_one, user_one)
    , ExperimenterDivested (experiment_two, user_two) ) ->
    equal experiment_one experiment_two
    && CCString.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created experiment | Updated experiment -> pp formatter experiment
  | Destroyed m -> Id.pp formatter m
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    pp formatter experiment;
    Admin.pp formatter user
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    ->
    pp formatter experiment;
    Admin.pp formatter user
;;
