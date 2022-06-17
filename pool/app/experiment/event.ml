open Entity

type create =
  { title : Title.t
  ; description : Description.t
  ; waiting_list_disabled : WaitingListDisabled.t
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; session_reminder_text : Pool_common.Reminder.Text.t option
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Updated of t * create
  | Destroyed of Common.Id.t
  | ExperimenterAssigned of t * Admin.experimenter Admin.t
  | ExperimenterDivested of t * Admin.experimenter Admin.t
  | AssistantAssigned of t * Admin.assistant Admin.t
  | AssistantDivested of t * Admin.assistant Admin.t

let handle_event pool : event -> unit Lwt.t = function
  | Created create_t ->
    create
      create_t.title
      create_t.description
      create_t.waiting_list_disabled
      create_t.direct_registration_disabled
      create_t.registration_disabled
      create_t.session_reminder_lead_time
      create_t.session_reminder_text
    |> Repo.insert pool
  | Updated (experiment, update_t) ->
    { experiment with
      title = update_t.title
    ; description = update_t.description
    ; waiting_list_disabled = update_t.waiting_list_disabled
    ; direct_registration_disabled = update_t.direct_registration_disabled
    ; registration_disabled = update_t.registration_disabled
    ; session_reminder_lead_time = update_t.session_reminder_lead_time
    ; session_reminder_text = update_t.session_reminder_text
    ; updated_at =
        Ptime_clock.now () (* TODO [timhub]: How to use SQL timestamp update? *)
    }
    |> Repo.update pool
  | Destroyed experiment_id -> Repo.destroy pool experiment_id
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    Permission.divest (Admin.user user) (Role.operator experiment.id)
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    -> Permission.divest (Admin.user user) (Role.operator experiment.id)
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | Created one, Created two -> equal_create one two
  | Updated (experiment_one, update_one), Updated (experiment_two, update_two)
    -> equal experiment_one experiment_two && equal_create update_one update_two
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
  | Created m -> pp_create formatter m
  | Updated (experiment, update) ->
    pp formatter experiment;
    pp_create formatter update
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
