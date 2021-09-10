open CCResult.Infix

module AddExperiment : sig
  type t =
    { title : string
    ; description : string
    }

  val handle : Experiment.create -> (Experiment.event, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { title : string
    ; description : string
    }

  let handle t =
    let open Experiment in
    let create =
      let open CCResult in
      let* title = Title.create t.title in
      let* description = Description.create t.description in
      Ok { title; description }
    in
    let event (t : Experiment.create) = Experiment.ExperimentAdded t in
    create >|= event
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Experiment ]
  ;;
end

module EditExperiment : sig
  type t =
    { experiment_id : string
    ; room : string
    ; building : string
    ; street : string
    ; zip : string
    ; city : string
    }

  val handle : t -> Experiment.t -> (Experiment.event list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { experiment_id : string
    ; room : string
    ; building : string
    ; street : string
    ; zip : string
    ; city : string
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
  type t = { experiment_id : string }

  val handle : t -> (Experiment.event list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { experiment_id : string }

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
  type t = { user_id : string }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Experiment.event, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

  let handle experiment user =
    Ok (Experiment.ExperimenterAssigned (experiment, user))
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module DivestExperimenter : sig
  type t =
    { user_id : string
    ; experiment_id : string
    }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Experiment.event, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; experiment_id : string
    }

  let handle experiment user =
    Ok (Experiment.ExperimenterDivested (experiment, user))
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
  type t = { user_id : string }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Experiment.event, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

  let handle experiment user =
    Ok (Experiment.AssistantAssigned (experiment, user))
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end

module DivestAssistant : sig
  type t =
    { user_id : string
    ; experiment_id : string
    }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Experiment.event, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; experiment_id : string
    }

  let handle experiment user =
    Ok (Experiment.AssistantDivested (experiment, user))
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
