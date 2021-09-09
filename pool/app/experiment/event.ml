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
  | ExperimenterAssignedToExperiment of t * Sihl_user.t
  | ExperimenterDivestedFromExperiment of t * Sihl_user.t
  | AssistantAssignedToExperiment of t * Sihl_user.t
  | AssistantDivestedFromExperiment of t * Sihl_user.t

let handle_event : event -> unit Lwt.t = function
  | ExperimentAdded create_t ->
    create create_t.title create_t.description |> Repo.insert
  | ExperimentEdited (experiment, update_t) ->
    let title = update_t.title in
    let description = update_t.description in
    { experiment with title; description } |> Repo.update
  | ExperimentDestroyed experiment -> Repo.destroy experiment
  | ExperimenterAssignedToExperiment (experiment, user)
  | ExperimenterDivestedFromExperiment (experiment, user)
  | AssistantAssignedToExperiment (experiment, user)
  | AssistantDivestedFromExperiment (experiment, user) ->
    Utils.todo (experiment, user)
;;

let equal_event event1 event2 =
  match event1, event2 with
  | ExperimentAdded one, ExperimentAdded two -> equal_create one two
  | ( ExperimentEdited (experiment_one, update_one)
    , ExperimentEdited (experiment_two, update_two) ) ->
    equal experiment_one experiment_two && equal_update update_one update_two
  | ExperimentDestroyed one, ExperimentDestroyed two -> equal one two
  | ( ExperimenterAssignedToExperiment (experiment_one, user_one)
    , ExperimenterDivestedFromExperiment (experiment_two, user_two) ) ->
    equal experiment_one experiment_two
    && String.equal user_one.Sihl_user.id user_two.Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | ExperimentAdded m -> pp_create formatter m
  | ExperimentEdited (experiment, update) ->
    let () = pp formatter experiment in
    pp_update formatter update
  | ExperimentDestroyed m -> pp formatter m
  | ExperimenterAssignedToExperiment (experiment, user)
  | ExperimenterDivestedFromExperiment (experiment, user)
  | AssistantAssignedToExperiment (experiment, user)
  | AssistantDivestedFromExperiment (experiment, user) ->
    let () = pp formatter experiment in
    Sihl_user.pp formatter user
;;
