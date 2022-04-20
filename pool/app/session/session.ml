module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if CCString.is_empty description
    then Error "Invalid description!"
    else Ok description
  ;;
end

module ParticipantAmount = struct
  type t = int [@@deriving eq, show]

  let create field max =
    let open CCResult.Infix in
    let open Pool_common.Message in
    max
    |> CCInt.of_string
    |> CCOption.to_result (Invalid field)
    >>= fun max -> if max < 0 then Error ParticipantAmountNegative else Ok max
  ;;

  let value m = m

  let schema field () =
    Pool_common.Utils.schema_decoder (create field) CCInt.to_string field
  ;;
end

module Start = struct
  type t = Ptime.t [@@deriving eq, show]

  let create date =
    let open CCResult.Infix in
    let session_datetime =
      date |> Ptime.of_date |> CCOption.to_result "Invalid session date"
    in
    let now = () |> Ptime_clock.now in
    let compare session_datetime =
      match Ptime.is_earlier session_datetime ~than:now with
      | true -> Error "The session date cannot be in the past"
      | false -> Ok session_datetime
    in
    session_datetime >>= compare
  ;;

  let value m = m
end

type t =
  { id : Pool_common.Id.t
  ; start : Ptime.t
  ; duration : Ptime.Span.t
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; participant_count : ParticipantAmount.t
  ; canceled_at : Ptime.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

type participations =
  { session : t
  ; participations : Participation.t list
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
