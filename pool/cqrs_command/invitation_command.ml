module Create : sig
  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    }

  val handle
    :  t
    -> Pool_common.Language.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    }

  let handle (command : t) language =
    let events, errors =
      CCList.fold_left
        (fun (events, errors) contact ->
          match
            CCList.mem
              ~eq:Pool_common.Id.equal
              (Contact.id contact)
              command.invited_contacts
          with
          | true -> events, errors @ [ Contact.fullname contact ]
          | false ->
            let create =
              Invitation.{ experiment = command.experiment; contact }
            in
            ( events
              @ [ (Invitation.Created create, language) |> Pool_event.invitation
                ]
            , errors ))
        ([], [])
        command.contacts
    in
    if CCList.is_empty errors
    then Ok events
    else Error Pool_common.Message.(AlreadyInvitedToExperiment errors)
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
