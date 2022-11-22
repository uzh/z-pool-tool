module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
module BaseGuard = Guard
open Mailing

let src = Logs.Src.create "mailing.cqrs"

module Create : sig
  include Common.CommandSig

  type t =
    { start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t
    ; distribution : Distribution.t option
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Id.t
    -> Experiment.t
    -> t
    -> (Pool_event.t list, Message.error) result

  val decode : Conformist.input -> (t, Message.error) result
  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
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
    ?(tags = Logs.Tag.empty)
    ?(id = Mailing.Id.create ())
    experiment
    ({ start_at; end_at; rate; distribution } : t)
    =
    Logs.info ~src (fun m -> m "Handle command CreateOperator" ~tags);
    let open CCResult in
    let* mailing = Mailing.create ~id start_at end_at rate distribution in
    Ok
      [ Mailing.Created (mailing, experiment.Experiment.id)
        |> Pool_event.mailing
      ]
  ;;

  let effects id =
    let _ = id in
    (* TODO [mabiede] All of with: [`Update, `Target (id |> Guard.Uuid.target_of
       Experiment.Id.value) ; `Update, `TargetEntity `Experiment] *)
    [ `Create, `TargetEntity `Mailing ]
  ;;
end

module Update : sig
  include Common.CommandSig with type t = Mailing.update

  val handle
    :  ?tags:Logs.Tag.set
    -> Mailing.t
    -> t
    -> (Pool_event.t list, Message.error) result

  val decode : Conformist.input -> (t, Message.error) result
  val effects : Mailing.Id.t -> BaseGuard.Authorizer.effect list
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

  let handle
    ?(tags = Logs.Tag.empty)
    ({ start_at; _ } as mailing : Mailing.t)
    (update : t)
    =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    match Ptime_clock.now () < Mailing.StartAt.value start_at with
    | true -> Ok [ Mailing.Updated (update, mailing) |> Pool_event.mailing ]
    | false -> Error Pool_common.Message.AlreadyStarted
  ;;

  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Mailing.Id.value)
    ; `Update, `TargetEntity `Mailing
    ]
  ;;
end

module Delete : sig
  include Common.CommandSig with type t = Mailing.t

  val effects : Mailing.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = Mailing.t

  let handle ?(tags = Logs.Tag.empty) (mailing : t) =
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    if StartAt.value mailing.start_at < Ptime_clock.now ()
    then Error Message.AlreadyInPast
    else Ok [ Deleted mailing |> Pool_event.mailing ]
  ;;

  let effects id =
    [ `Delete, `Target (id |> BaseGuard.Uuid.target_of Mailing.Id.value)
    ; `Delete, `TargetEntity `Mailing
    ]
  ;;
end

module Stop : sig
  include Common.CommandSig with type t = Mailing.t

  val effects : Mailing.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = Mailing.t

  let handle ?(tags = Logs.Tag.empty) (mailing : t) =
    Logs.info ~src (fun m -> m "Handle command Stop" ~tags);
    let now = Ptime_clock.now () in
    if StartAt.value mailing.start_at < now && now < EndAt.value mailing.end_at
    then Ok [ Mailing.Stopped mailing |> Pool_event.mailing ]
    else Error Message.NotInTimeRange
  ;;

  let effects id =
    [ `Update, `Target (id |> BaseGuard.Uuid.target_of Mailing.Id.value)
    ; `Update, `TargetEntity `Mailing
    ]
  ;;
end

module Overlaps : sig
  include Common.CommandSig

  type t =
    { id : Id.t option
    ; start_at : StartAt.t
    ; end_at : EndAt.t
    ; rate : Rate.t option
    ; distribution : Distribution.t option
    }

  type with_default_rate = bool

  val handle : t -> (with_default_rate * Mailing.t, Message.error) result
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

  let handle ({ id; start_at; end_at; rate; distribution } : t) =
    let open CCResult in
    Mailing.create
      ?id
      start_at
      end_at
      (CCOption.get_or ~default:Mailing.Rate.default rate)
      distribution
    >|= fun m -> CCOption.is_none rate, m
  ;;

  let effects = [ `Read, `TargetEntity `Mailing ]
end
