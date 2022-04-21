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
  | Created of create
  | Updated of t * update
  | Destroyed of t
  | ExperimenterAssigned of t * Admin.experimenter Admin.t
  | ExperimenterDivested of t * Admin.experimenter Admin.t
  | AssistantAssigned of t * Admin.assistant Admin.t
  | AssistantDivested of t * Admin.assistant Admin.t

let handle_event pool : event -> unit Lwt.t = function
  | Created create_t ->
    create create_t.title create_t.description |> Repo.insert pool
  | Updated (experiment, update_t) ->
    { experiment with
      title = update_t.title
    ; description = update_t.description
    }
    |> Repo.update pool
  | Destroyed experiment -> Repo.destroy pool experiment
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
    -> equal experiment_one experiment_two && equal_update update_one update_two
  | Destroyed one, Destroyed two -> equal one two
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
    pp_update formatter update
  | Destroyed m -> pp formatter m
  | ExperimenterAssigned (experiment, user)
  | ExperimenterDivested (experiment, user) ->
    pp formatter experiment;
    Admin.pp formatter user
  | AssistantAssigned (experiment, user) | AssistantDivested (experiment, user)
    ->
    pp formatter experiment;
    Admin.pp formatter user
;;
