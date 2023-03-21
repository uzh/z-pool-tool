module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
module BaseGuard = Guard
open Mailing

type create =
  { start_at : StartAt.t option
  ; start_now : StartNow.t
  ; end_at : EndAt.t
  ; rate : Rate.t
  ; distribution : Distribution.t option
  }
[@@deriving eq, show]

let src = Logs.Src.create "mailing.cqrs"

let default_command start_at start_now end_at rate random distribution : create =
  let distribution =
    let open Distribution in
    if random then Some Random else distribution |> CCOption.map create_sorted
  in
  { start_at; start_now; end_at; rate; distribution }
;;

let defalt_schema =
  Conformist.(
    make
      Field.
        [ Conformist.optional @@ StartAt.schema ()
        ; StartNow.schema ()
        ; EndAt.schema ()
        ; Rate.schema ()
        ; Distribution.is_random_schema ()
        ; Conformist.optional @@ Distribution.schema ()
        ]
      default_command)
;;

module Create : sig
  include Common.CommandSig with type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Id.t
    -> Experiment.t
    -> t
    -> (Pool_event.t list, Message.error) result

  val decode : Conformist.input -> (create, Message.error) result
  val effects : Experiment.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = create

  let decode data =
    Conformist.decode_and_validate defalt_schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?(id = Mailing.Id.create ())
    experiment
    ({ start_at; start_now; end_at; rate; distribution } : t)
    =
    Logs.info ~src (fun m -> m "Handle command CreateOperator" ~tags);
    let open CCResult in
    let* start = Start.create start_at start_now in
    let* mailing = Mailing.create ~id start end_at rate distribution in
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
  include Common.CommandSig with type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> Mailing.t
    -> t
    -> (Pool_event.t list, Message.error) result

  val decode : Conformist.input -> (create, Message.error) result
  val effects : Mailing.Id.t -> BaseGuard.Authorizer.effect list
end = struct
  type t = create

  let decode data =
    Conformist.decode_and_validate defalt_schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    (mailing : Mailing.t)
    ({ start_at; start_now; end_at; rate; distribution } : t)
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let* start_at =
      Start.create start_at start_now
      >>= CCFun.flip Mailing.Start.validate end_at
    in
    let update = { start_at; end_at; rate; distribution } in
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

  let command id start_at end_at rate random distribution : t =
    let distribution =
      let open Distribution in
      if random then Some Random else distribution |> CCOption.map create_sorted
    in
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
          ; Distribution.is_random_schema ()
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
      Start.(StartAt start_at)
      end_at
      (CCOption.get_or ~default:Mailing.Rate.default rate)
      distribution
    >|= fun m -> CCOption.is_none rate, m
  ;;

  let effects = [ `Read, `TargetEntity `Mailing ]
end
