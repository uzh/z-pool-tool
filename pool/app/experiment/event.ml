open Entity

type create =
  { title : Title.t
  ; description : Description.t
  }
[@@deriving eq, show]

type update =
  { title : Title.t
  ; description : Description.t
  }
[@@deriving eq, show]

type event =
  | ExperimentAdded of create
  | ExperimentEdited of t * update
  | ExperimentDestroyed of t
  | ExperimenterAssigned of t * Admin.experimenter Admin.t
  | ExperimenterDivested of t * Admin.experimenter Admin.t
  | AssistantAssigned of t * Admin.assistant Admin.t
  | AssistantDivested of t * Admin.assistant Admin.t

let handle_event pool : event -> unit Lwt.t = function
  | ExperimentAdded create_t ->
    create create_t.title create_t.description |> Repo.insert pool
  | ExperimentEdited (experiment, update_t) ->
    { experiment with
      title = update_t.title
    ; description = update_t.description
    }
    |> Repo.update pool
  | ExperimentDestroyed experiment -> Repo.destroy pool experiment
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    Permission.divest (Admin.user user) (Role.operator experiment.id)
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    -> Permission.divest (Admin.user user) (Role.operator experiment.id)
;;

let equal_event event1 event2 =
  match event1, event2 with
  | ExperimentAdded one, ExperimentAdded two -> equal_create one two
  | ( ExperimentEdited (experiment_one, update_one)
    , ExperimentEdited (experiment_two, update_two) ) ->
    equal experiment_one experiment_two && equal_update update_one update_two
  | ExperimentDestroyed one, ExperimentDestroyed two -> equal one two
  | ( ExperimenterAssigned (experiment_one, user_one)
    , ExperimenterDivested (experiment_two, user_two) ) ->
    equal experiment_one experiment_two
    && CCString.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | ( ( ExperimentAdded _
      | ExperimentEdited _
      | ExperimentDestroyed _
      | ExperimenterAssigned _
      | ExperimenterDivested _
      | AssistantAssigned _
      | AssistantDivested _ )
    , _ ) -> false
;;

let pp_event formatter event =
  match event with
  | ExperimentAdded m -> pp_create formatter m
  | ExperimentEdited (experiment, update) ->
    pp formatter experiment;
    pp_update formatter update
  | ExperimentDestroyed m -> pp formatter m
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    pp formatter experiment;
    Admin.pp formatter user
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    ->
    pp formatter experiment;
    Admin.pp formatter user
;;
