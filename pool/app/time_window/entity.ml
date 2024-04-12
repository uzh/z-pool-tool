open Session

type t =
  { id : Id.t
  ; start : Start.t
  ; duration : Duration.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; max_participants : ParticipantAmount.t option
  ; assignment_count : AssignmentCount.t
  ; no_show_count : NoShowCount.t
  ; participant_count : ParticipantCount.t
  ; closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; experiment : Experiment.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?id
  ?internal_description
  ?public_description
  ?max_participants
  start
  duration
  experiment
  =
  let count encode = encode 0 |> Pool_common.Utils.get_or_failwith in
  { id = id |> CCOption.value ~default:(Id.create ())
  ; start
  ; duration
  ; internal_description
  ; public_description
  ; max_participants
  ; assignment_count = count AssignmentCount.create
  ; no_show_count = count NoShowCount.create
  ; participant_count = count ParticipantCount.create
  ; closed_at = None
  ; canceled_at = None
  ; experiment
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

let ends_at ({ start; duration; _ } : t) =
  Ptime.add_span (Start.value start) (Duration.value duration)
  |> CCOption.to_result Pool_common.Message.(Invalid Field.Duration)
  |> Pool_common.Utils.get_or_failwith
;;

let duration ~start ~end_at =
  let open CCResult.Infix in
  let open Ptime in
  to_float_s (End.value end_at) -. to_float_s (Start.value start)
  |> Span.of_float_s
  |> CCOption.to_result Pool_common.Message.(Invalid Field.End)
  >>= Duration.create
;;

let has_assignments (m : t) = AssignmentCount.value m.assignment_count > 0
let is_deletable (m : t) = m |> has_assignments |> not

let not_canceled ({ canceled_at; _ } : t) =
  match canceled_at with
  | None -> Ok ()
  | Some canceled_at -> Session.is_canceled_error canceled_at
;;

let not_closed ({ closed_at; _ } : t) =
  let open Pool_common.Message in
  match closed_at with
  | None -> Ok ()
  | Some closed_at ->
    closed_at
    |> Pool_common.Utils.Time.formatted_date_time
    |> sessionalreadyclosed
    |> CCResult.fail
;;

let not_closed_or_canceled session =
  let open CCResult.Infix in
  let* () = not_closed session in
  let* () = not_canceled session in
  Ok ()
;;

let is_closable = not_closed_or_canceled

let start_end_with_duration_human ({ start; duration; _ } : t) =
  Utils.Ptime.format_start_end (Start.value start) (Duration.value duration)
;;
