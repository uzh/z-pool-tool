module Create : sig
  type t =
    { experiment : Experiment.t
    ; subject : Subject.t
    }

  val handle
    :  t
    -> Pool_common.Language.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { experiment : Experiment.t
    ; subject : Subject.t
    }

  let handle (command : t) language =
    let (create : Invitation.create) =
      Invitation.{ experiment = command.experiment; subject = command.subject }
    in
    Ok [ (Invitation.Created create, language) |> Pool_event.invitation ]
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
  val handle
    :  Invitation.resent
    -> Pool_common.Language.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> Invitation.resent -> bool Lwt.t
end = struct
  let handle t language =
    Ok [ (Invitation.Resent t, language) |> Pool_event.invitation ]
  ;;

  let can user Invitation.{ experiment; invitation } =
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
