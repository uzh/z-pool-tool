open CCResult.Infix

module AddExperiment : sig
  type t =
    { title : string
    ; description : string
    }

  val handle : Experiment.create -> (Pool_event.t list, string) result
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
    let event (t : Experiment.create) =
      [ Experiment.ExperimentAdded t |> Pool_event.experiment ]
    in
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

  val handle : t -> Experiment.t -> (Pool_event.t list, string) result
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
        [ Permission.Update
            ( Permission.Experiment
            , Some (command.experiment_id |> Common.Id.of_string) )
        ]
  ;;
end

module DestroyExperiment : sig
  type t = { experiment_id : string }

  val handle : t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { experiment_id : string }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy
            ( Permission.Experiment
            , Some (command.experiment_id |> Common.Id.of_string) )
        ]
  ;;
end

module AddExperimenter : sig
  type t = { user_id : string }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

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
    { user_id : string
    ; experiment_id : string
    }

  val handle
    :  Experiment.t
    -> Admin.experimenter Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; experiment_id : string
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
        ; Permission.Manage
            ( Permission.Experiment
            , Some (command.experiment_id |> Common.Id.of_string) )
        ]
  ;;
end

module AddAssistant : sig
  type t = { user_id : string }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = { user_id : string }

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
    { user_id : string
    ; experiment_id : string
    }

  val handle
    :  Experiment.t
    -> Admin.assistant Admin.t
    -> (Pool_event.t list, 'a) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { user_id : string
    ; experiment_id : string
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
        ; Permission.Manage
            ( Permission.Experiment
            , Some (command.experiment_id |> Common.Id.of_string) )
        ]
  ;;
end
