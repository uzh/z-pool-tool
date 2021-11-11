open Experiment
module Id = Pool_common.Id

module AddExperiment : sig
  type t =
    { title : Experiment.Title.t
    ; description : Experiment.Description.t
    }

  val handle : t -> (Pool_event.t list, 'a) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : Experiment.Title.t
    ; description : Experiment.Description.t
    }

  let handle (command : t) =
    let create : Experiment.create =
      { title = command.title; description = command.description }
    in
    let event (t : Experiment.create) =
      Ok [ Experiment.ExperimentAdded t |> Pool_event.experiment ]
    in
    create |> event
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Experiment ]
  ;;
end

module EditExperiment : sig
  type t =
    { experiment_id : Id.t
    ; room : Experiment.Location.Room.t
    ; building : Experiment.Location.Building.t
    ; street : Experiment.Location.Street.t
    ; zip : Experiment.Location.Zip.t
    ; city : Experiment.Location.City.t
    }

  val handle : t -> Experiment.t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { experiment_id : Id.t
    ; room : Experiment.Location.Room.t
    ; building : Experiment.Location.Building.t
    ; street : Experiment.Location.Street.t
    ; zip : Experiment.Location.Zip.t
    ; city : Experiment.Location.City.t
    }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Experiment, Some command.experiment_id)
        ]
  ;;
end

module DestroyExperiment : sig
  type t = { experiment_id : Id.t }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { experiment_id : Id.t }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy (Permission.Experiment, Some command.experiment_id)
        ]
  ;;
end

module AddExperimenter : sig
  type t = { user_id : Id.t }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : Id.t }

  let handle experiment user =
    Ok
      [ Experiment.ExperimenterAssigned (experiment, user)
        |> Pool_event.experiment
      ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module DivestExperimenter : sig
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  let handle experiment user =
    Ok
      [ Experiment.ExperimenterDivested (experiment, user)
        |> Pool_event.experiment
      ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage (Permission.Experiment, Some command.experiment_id)
        ]
  ;;
end

module AddAssistant : sig
  type t = { user_id : Id.t }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : Id.t }

  let handle experiment user =
    Ok
      [ Experiment.AssistantAssigned (experiment, user) |> Pool_event.experiment
      ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module DivestAssistant : sig
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : Id.t
    ; experiment_id : Id.t
    }

  let handle experiment user =
    Ok
      [ Experiment.AssistantDivested (experiment, user) |> Pool_event.experiment
      ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage (Permission.System, None)
        ; Permission.Manage (Permission.Experiment, Some command.experiment_id)
        ]
  ;;
end
