module Create : sig
  type t =
    { experiment : Experiment.t
    ; participant : Participant.t
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { experiment : Experiment.t
    ; participant : Participant.t
    }

  let handle (command : t) =
    let create =
      Invitation.
        { experiment_id = command.experiment.Experiment.id
        ; participant = command.participant
        }
    in
    Ok [ Invitation.Created create |> Pool_event.invitation ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            (Permission.Experiment, Some command.experiment.Experiment.id)
        ; Permission.Create Permission.Invitation
        ]
  ;;
end

module Resend : sig
  type t = Invitation.t

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> Experiment.t -> t -> bool Lwt.t
end = struct
  type t = Invitation.t

  let handle invitation =
    Ok [ Invitation.Resent invitation |> Pool_event.invitation ]
  ;;

  let can user experiment invitation =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            (Permission.Experiment, Some experiment.Experiment.id)
        ; Permission.Manage
            (Permission.Invitation, Some invitation.Invitation.id)
        ]
  ;;
end
