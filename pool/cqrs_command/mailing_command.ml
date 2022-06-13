module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
open Mailing

module Create : sig
  type t =
    { start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t
    ; distribution : Distribution.t option
    }

  val handle
    :  ?id:Id.t
    -> Experiment.t
    -> t
    -> (Pool_event.t list, Message.error) result

  val decode : Conformist.input -> (t, Message.error) result
  val can : Sihl_user.t -> Experiment.t -> bool Lwt.t
end = struct
  type t =
    { start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t
    ; distribution : Distribution.t option
    }

  let command start_at end_at rate distribution : t =
    { start_at; end_at; rate; distribution }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ StartAt.schema ()
          ; EndAt.schema ()
          ; Rate.schema ()
          ; Conformist.optional @@ Distribution.schema ()
          ]
        command)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let handle
      ?(id = Mailing.Id.create ())
      experiment
      ({ start_at; end_at; rate; distribution } : t)
    =
    let open CCResult in
    let* mailing = Mailing.create ~id start_at end_at rate distribution in
    Ok
      [ Mailing.Created (mailing, experiment.Experiment.id)
        |> Pool_event.mailing
      ]
  ;;

  let can user experiment =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            (Permission.Experiment, Some experiment.Experiment.id)
        ; Permission.Create Permission.Mailing
        ]
  ;;
end

module Update = struct end
module Delete = struct end
module Stop = struct end
