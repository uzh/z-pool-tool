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

  let create m = m
  let value m = m

  let schema () =
    let decode str =
      let open CCResult in
      Pool_common.(Utils.Time.parse_time str >|= create)
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
    let decode str = Pool_common.(Utils.Time.parse_time_span str >>= create) in
    let encode span = Pool_common.Utils.Time.print_time_span span in
    Pool_common.(Utils.schema_decoder decode encode Message.Field.Duration)
  ;;
end

module AssignmentCount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.AssignmentCount)
    else Ok m
  ;;
end

type t =
  { id : Pool_common.Id.t
  ; start : Start.t
  ; duration : Ptime.Span.t
  ; description : Description.t option
  ; location : Pool_location.t
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; assignment_count : AssignmentCount.t
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
    location
    max_participants
    min_participants
    overbook
  =
  { id = id |> CCOption.value ~default:(Pool_common.Id.create ())
  ; start
  ; duration
  ; description
  ; location
  ; max_participants
  ; min_participants
  ; overbook
  ; assignment_count = 0
  ; canceled_at = None
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

let is_fully_booked (m : t) =
  m.assignment_count >= m.max_participants + m.overbook
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

let to_email_text language start duration location =
  let format label text =
    Format.asprintf
      "%s: %s"
      Pool_common.(Utils.field_to_string language label)
      text
  in
  let start =
    format
      Pool_common.Message.Field.Start
      (Start.value start |> Pool_common.Utils.Time.formatted_date_time)
  in
  let duration =
    format
      Pool_common.Message.Field.Duration
      (Duration.value duration |> Pool_common.Utils.Time.formatted_timespan)
  in
  let location =
    format
      Pool_common.Message.Field.Location
      (Pool_location.to_string language location)
  in
  CCString.concat "\n" [ start; duration; location ]
;;

let session_date_to_human (session : t) =
  session.start |> Start.value |> Pool_common.Utils.Time.formatted_date_time
;;

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; start : Start.t
    ; duration : Ptime.Span.t
    ; description : Description.t option
    ; location : Pool_location.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; canceled_at : Ptime.t option
    }
  [@@deriving eq, show]

  let is_fully_booked (m : t) =
    m.assignment_count >= m.max_participants + m.overbook
  ;;
end
