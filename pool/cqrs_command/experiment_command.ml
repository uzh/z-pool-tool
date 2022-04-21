open Experiment
module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_common.Id

module Create : sig
  type t = create

  val handle : t -> (Pool_event.t list, 'a) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = create

  let command title description = { title; description }

  let schema =
    Pool_common.Utils.PoolConformist.(
      make Field.[ Title.schema (); Description.schema () ] command)
  ;;

  let handle (command : t) =
    let t = { title = command.title; description = command.description } in
    Ok [ Experiment.Created t |> Pool_event.experiment ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_coformist_error
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Experiment ]
  ;;
end

module Update : sig end = struct end

module Delete : sig
  type t =
    { experiment_id : Id.t
    ; session_count : int
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  (* Only when no sessions added *)

  type t =
    { experiment_id : Id.t
    ; session_count : int
    }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Destroy (Permission.Experiment, Some command.experiment_id)
        ]
  ;;
end

module UpdateFilter : sig end = struct
  (* Update 'match_filter' flag in currently existing participations *)
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
