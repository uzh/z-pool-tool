module Conformist = Pool_common.Utils.PoolConformist

module Create : sig
  type t =
    { participant : Participant.t
    ; session : Session.t
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { participant : Participant.t
    ; session : Session.t
    }

  let handle (command : t) =
    let create =
      Participation.
        { participant = command.participant
        ; session_id = command.session.Session.id
        }
    in
    Ok [ Participation.Created create |> Pool_event.participation ]
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Create Permission.Participation ]
  ;;
end

module Cancel : sig
  type t = Participation.t

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Participation.t

  let handle (command : t)
      : (Pool_event.t list, Pool_common.Message.error) result
    =
    Ok [ Participation.Canceled command |> Pool_event.participation ]
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Update
            (Permission.Participation, Some command.Participation.id)
        ]
  ;;
end

module SetAttendance : sig
  type t =
    { show_up : Participation.ShowUp.t
    ; participated : Participation.Participated.t
    }

  val handle
    :  Participation.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> Participation.t -> bool Lwt.t
end = struct
  type t =
    { show_up : Participation.ShowUp.t
    ; participated : Participation.Participated.t
    }

  let command (show_up : Participation.ShowUp.t) participated =
    { show_up; participated }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Participation.ShowUp.schema ()
          ; Participation.Participated.schema ()
          ]
        command)
  ;;

  let handle participation (command : t) =
    Ok
      [ Participation.ShowedUp (participation, command.show_up)
        |> Pool_event.participation
      ; Participation.Participated (participation, command.participated)
        |> Pool_event.participation
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_coformist_error
  ;;

  let can user participation =
    Permission.can
      user
      ~any_of:
        [ Permission.Update
            (Permission.Participation, Some participation.Participation.id)
        ]
  ;;
end
