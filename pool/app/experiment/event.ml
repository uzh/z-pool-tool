open Entity

type event =
  | ExperimentAdded of t [@equal equal]
  | ExperimentEdited of t
  | ExperimentDestroyed of t
  | ExperimenterAssignedToExperiment of t * Sihl_user.t
  | ExperimenterDivestedFromExperiment of t * Sihl_user.t
  | AssistantAssignedToExperiment of t * Sihl_user.t
  | AssistantDivestedFromExperiment of t * Sihl_user.t

let handle_event : event -> unit Lwt.t = function
  | ExperimentAdded experiment -> Repo.insert experiment
  | ExperimentEdited experiment -> Repo.update experiment
  | ExperimentDestroyed experiment -> Repo.destroy experiment
  | ExperimenterAssignedToExperiment (experiment, user)
  | ExperimenterDivestedFromExperiment (experiment, user)
  | AssistantAssignedToExperiment (experiment, user)
  | AssistantDivestedFromExperiment (experiment, user) ->
    Utils.todo (experiment, user)
;;

let equal_event event1 event2 =
  match event1, event2 with
  | ExperimentAdded one, ExperimentAdded two
  | ExperimentEdited one, ExperimentEdited two
  | ExperimentDestroyed one, ExperimentDestroyed two -> equal one two
  | ( ExperimenterAssignedToExperiment (experiment_one, user_one)
    , ExperimenterDivestedFromExperiment (experiment_two, user_two) ) ->
    equal experiment_one experiment_two
    && String.equal user_one.Sihl_user.id user_two.Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | ExperimentAdded m | ExperimentEdited m | ExperimentDestroyed m ->
    pp formatter m
  | ExperimenterAssignedToExperiment (experiment, user)
  | ExperimenterDivestedFromExperiment (experiment, user)
  | AssistantAssignedToExperiment (experiment, user)
  | AssistantDivestedFromExperiment (experiment, user) ->
    let () = pp formatter experiment in
    Sihl_user.pp formatter user
;;
