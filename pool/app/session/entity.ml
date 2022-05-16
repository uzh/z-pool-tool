module type Base = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if CCString.is_empty description
    then Error Pool_common.Message.NoValue
    else Ok description
  ;;

  let schema () =
    Pool_common.(Utils.schema_decoder create value Message.Field.Description)
  ;;
end

module ParticipantAmount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create amount =
    if amount < 0 then Error Pool_common.Message.(NegativeAmount) else Ok amount
  ;;

  let schema field =
    let decode str =
      let open CCResult in
      CCInt.of_string str
      |> CCOption.to_result Pool_common.Message.(NotANumber str)
      >>= create
    in
    Pool_common.(Utils.schema_decoder decode CCInt.to_string field)
  ;;
end

module Start = struct
  type t = Ptime.t [@@deriving eq, show]

  let create start =
    let now = () |> Ptime_clock.now in
    match Ptime.is_earlier start ~than:now with
    | true -> Error Pool_common.Message.TimeInPast
    | false -> Ok start
  ;;

  let value m = m

  let schema () =
    let decode str =
      let open CCResult in
      Pool_common.(Utils.parse_time str >>= create)
    in
    Pool_common.(
      Utils.schema_decoder decode Ptime.to_rfc3339 Message.Field.Start)
  ;;
end

module Duration = struct
  type t = Ptime.Span.t [@@deriving eq, show]

  let create m =
    if Ptime.Span.abs m |> Ptime.Span.equal m
    then Ok m
    else Error Pool_common.Message.NegativeAmount
  ;;

  let value m = m

  let schema () =
    let open CCResult in
    let decode str = Pool_common.(Utils.parse_time_span str >>= create) in
    let encode span = Pool_common.Utils.print_time_span span in
    Pool_common.(Utils.schema_decoder decode encode Message.Field.Duration)
  ;;
end

type t =
  { id : Pool_common.Id.t
  ; start : Start.t
  ; duration : Ptime.Span.t
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; (* TODO [aerben] want multiple follow up session?
     * 1. Ja es gibt immer wieder Sessions mit mehreren Following Sessions
     * 2. Eigentlich ist es immer eine Hauptsession mit mehreren Following Sessions

     * Could this model as the following, just flatten tail of linked list
     *  : ; follow_up : t *)
    (* TODO [aerben] make type for canceled_at? *)
    canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

(* TODO [aerben] need insertion multiple session? *)
(* TODO [aerben] do session copying *)
(* TODO [aerben] write tests *)

let create
    ?id
    start
    duration
    description
    max_participants
    min_participants
    overbook
  =
  { id = id |> CCOption.value ~default:(Pool_common.Id.create ())
  ; start
  ; duration
  ; description
  ; max_participants
  ; min_participants
  ; overbook
  ; canceled_at = None
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

type assignments =
  { session : t
  ; assignments : Assignment.t list
  }

type notification_log =
  | Email of Sihl_email.t * Sihl_queue.instance
  | SMS of string * Sihl_queue.instance
(* TODO: update notification history with types, add equal and pp functions *)

type notification_history =
  { session : t
  ; queue_entries : (Sihl_email.t * Sihl_queue.instance) list
        [@equal fun _ _ -> true]
  }

let find_by_experiment (_ : string) : t list Lwt.t = Lwt.return []

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; start : Start.t
    ; duration : Ptime.Span.t
    ; description : Description.t option
    ; canceled_at : Ptime.t option
    }
  [@@deriving eq, show]
end
