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

module Update : sig
  type t = Mailing.update

  val handle : Mailing.t -> t -> (Pool_event.t list, Message.error) result
  val decode : Conformist.input -> (t, Message.error) result
  val can : Sihl_user.t -> Mailing.t -> bool Lwt.t
end = struct
  type t = Mailing.update

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

  let handle ({ start_at; _ } as mailing : Mailing.t) (update : t) =
    match Ptime_clock.now () < Mailing.StartAt.value start_at with
    | true -> Ok [ Mailing.Updated (update, mailing) |> Pool_event.mailing ]
    | false -> Error Pool_common.Message.AlreadyStarted
  ;;

  let can user mailing =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            ( Permission.Mailing
            , Some
                (mailing.Mailing.id
                |> Mailing.Id.value
                |> Pool_common.Id.of_string) )
        ]
  ;;
end

module Delete : sig
  type t = Mailing.t

  val handle
    :  Mailing.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Mailing.t

  let handle (mailing : t) =
    if StartAt.value mailing.start_at < Ptime_clock.now ()
    then Error Message.AlreadyInPast
    else Ok [ Deleted mailing |> Pool_event.mailing ]
  ;;

  let can user mailing =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            ( Permission.Mailing
            , Some (mailing.Mailing.id |> Id.value |> Pool_common.Id.of_string)
            )
        ]
  ;;
end

module Stop : sig
  type t = Mailing.t

  val handle
    :  Mailing.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t = Mailing.t

  let handle (mailing : t) =
    let now = Ptime_clock.now () in
    if StartAt.value mailing.start_at < now && now < EndAt.value mailing.end_at
    then Ok [ Mailing.Stopped mailing |> Pool_event.mailing ]
    else Error Message.NotInTimeRange
  ;;

  let can user mailing =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            ( Permission.Mailing
            , Some (mailing.Mailing.id |> Id.value |> Pool_common.Id.of_string)
            )
        ]
  ;;
end

module Overlaps : sig
  type t =
    { id : Id.t option
    ; start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t option
    ; distribution : Distribution.t option
    }

  type with_default_rate = bool

  val create : t -> (with_default_rate * Mailing.t, Message.error) result
  val decode : Conformist.input -> (t, Message.error) result
end = struct
  type t =
    { id : Id.t option
    ; start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t option
    ; distribution : Distribution.t option
    }

  type with_default_rate = bool

  let command id start_at end_at rate distribution : t =
    { id; start_at; end_at; rate; distribution }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Conformist.optional @@ Id.schema ()
          ; StartAt.schema ()
          ; EndAt.schema ()
          ; Conformist.optional @@ Rate.schema ()
          ; Conformist.optional @@ Distribution.schema ()
          ]
        command)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let create ({ id; start_at; end_at; rate; distribution } : t) =
    let open CCResult in
    Mailing.create
      ?id
      start_at
      end_at
      (CCOption.get_or ~default:Mailing.Rate.default rate)
      distribution
    >|= fun m -> CCOption.is_none rate, m
  ;;
end
