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

let handle_event : event -> unit Lwt.t = function
  | ExperimentAdded create_t ->
    create create_t.title create_t.description |> Repo.insert
  | ExperimentEdited (experiment, update_t) ->
    { experiment with
      title = update_t.title
    ; description = update_t.description
    }
    |> Repo.update
  | ExperimentDestroyed experiment -> Repo.destroy experiment
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    Permission.divest
      (Admin.user user)
      (Role.operator (experiment.id |> Id.to_human))
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    ->
    Permission.divest
      (Admin.user user)
      (Role.operator (experiment.id |> Id.to_human))
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
    && String.equal
         (Admin.user user_one).Sihl_user.id
         (Admin.user user_two).Sihl_user.id
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | ExperimentAdded m -> pp_create formatter m
  | ExperimentEdited (experiment, update) ->
    let () = pp formatter experiment in
    pp_update formatter update
  | ExperimentDestroyed m -> pp formatter m
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    let () = pp formatter experiment in
    Admin.pp formatter user
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    ->
    let () = pp formatter experiment in
    Admin.pp formatter user
;;
