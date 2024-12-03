module Conformist = Pool_conformist
module Message = Pool_message
module BaseGuard = Guard
open Mailing

type create =
  { start_at : StartAt.t option
  ; start_now : StartNow.t
  ; end_at : EndAt.t
  ; limit : Limit.t
  ; distribution : Distribution.t option
  }
[@@deriving eq, show]

let src = Logs.Src.create "mailing.cqrs"

let mailing_effect action id =
  let open BaseGuard in
  ValidationSet.one_of_tuple
    (action, `Mailing, Some (id |> Uuid.target_of Mailing.Id.value))
;;

let default_command start_at start_now end_at limit random distribution : create =
  let distribution =
    let open Distribution in
    if random then Some Random else distribution |> CCOption.map create_sorted
  in
  { start_at; start_now; end_at; limit; distribution }
;;

let defalt_schema =
  Conformist.(
    make
      Field.
        [ Conformist.optional @@ StartAt.schema ()
        ; StartNow.schema ()
        ; EndAt.schema ()
        ; Limit.schema ()
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
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : Conformist.input -> (create, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = create

  let decode data =
    Conformist.decode_and_validate defalt_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let handle
        ?(tags = Logs.Tag.empty)
        ?(id = Mailing.Id.create ())
        experiment
        ({ start_at; start_now; end_at; limit; distribution } : t)
    =
    Logs.info ~src (fun m -> m "Handle command CreateOperator" ~tags);
    let open CCResult in
    let* start = Start.create start_at start_now in
    let* mailing = Mailing.create ~id start end_at limit distribution in
    Ok [ Mailing.Created (mailing, experiment.Experiment.id) |> Pool_event.mailing ]
  ;;

  let effects = Mailing.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = create

  val handle
    :  ?tags:Logs.Tag.set
    -> Mailing.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : Conformist.input -> (create, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> Mailing.Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = create

  let decode data =
    Conformist.decode_and_validate defalt_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let handle
        ?(tags = Logs.Tag.empty)
        (mailing : Mailing.t)
        ({ start_at; start_now; end_at; limit; distribution } : t)
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    let* start_at =
      Start.create start_at start_now >>= CCFun.flip Mailing.Start.validate end_at
    in
    let update = { start_at; end_at; limit; distribution } in
    match Ptime_clock.now () < Mailing.StartAt.value start_at with
    | true -> Ok [ Mailing.Updated (update, mailing) |> Pool_event.mailing ]
    | false -> Error Pool_message.Error.AlreadyStarted
  ;;

  let effects = Mailing.Guard.Access.update
end

module Delete : sig
  include Common.CommandSig with type t = Mailing.t

  val effects : Experiment.Id.t -> Mailing.Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = Mailing.t

  let handle ?(tags = Logs.Tag.empty) (mailing : t) =
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    if StartAt.value mailing.start_at < Ptime_clock.now ()
    then Error Pool_message.Error.AlreadyInPast
    else Ok [ Deleted mailing |> Pool_event.mailing ]
  ;;

  let effects = Mailing.Guard.Access.delete
end

module Stop : sig
  include Common.CommandSig with type t = Mailing.t

  val effects : Experiment.Id.t -> Mailing.Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t = Mailing.t

  let handle ?(tags = Logs.Tag.empty) (mailing : t) =
    Logs.info ~src (fun m -> m "Handle command Stop" ~tags);
    let now = Ptime_clock.now () in
    if StartAt.value mailing.start_at < now && now < EndAt.value mailing.end_at
    then Ok [ Mailing.Stopped mailing |> Pool_event.mailing ]
    else Error Pool_message.Error.NotInTimeRange
  ;;

  let effects = Mailing.Guard.Access.update
end

module Overlaps : sig
  include Common.CommandSig

  type t =
    { id : Id.t option
    ; start_at : StartAt.t
    ; end_at : EndAt.t
    ; limit : Limit.t option
    ; distribution : Distribution.t option
    }

  val handle : t -> (Mailing.t, Pool_message.Error.t) result
  val decode : Conformist.input -> (t, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t =
    { id : Id.t option
    ; start_at : StartAt.t
    ; end_at : EndAt.t
    ; limit : Limit.t option
    ; distribution : Distribution.t option
    }

  let command id start_at start_now end_at limit random distribution : t =
    let start_at =
      let default = StartAt.create_now () in
      if StartNow.value start_now then default else CCOption.value ~default start_at
    in
    let distribution =
      let open Distribution in
      if random then Some Random else distribution |> CCOption.map create_sorted
    in
    { id; start_at; end_at; limit; distribution }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Conformist.optional @@ Id.schema ()
          ; Conformist.optional @@ StartAt.schema ()
          ; StartNow.schema ()
          ; EndAt.schema ()
          ; Conformist.optional @@ Limit.schema ()
          ; Distribution.is_random_schema ()
          ; Conformist.optional @@ Distribution.schema ()
          ]
        command)
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let handle ({ id; start_at; end_at; limit; distribution } : t) =
    Mailing.create
      ~allow_start_in_past:true
      ?id
      Start.(StartAt start_at)
      end_at
      (CCOption.get_or ~default:Mailing.Limit.default limit)
      distribution
  ;;

  let effects = Mailing.Guard.Access.index
end
