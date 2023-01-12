open Entity

type base =
  { start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }
[@@deriving eq, show]

type update =
  { start : Start.t option
  ; duration : Duration.t option
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }
[@@deriving eq, show]

type reschedule =
  { start : Start.t
  ; duration : Duration.t
  }
[@@deriving eq, show]

(* TODO [aerben] experiment ID *)
type event =
  | Created of
      (base * Pool_common.Id.t option * Experiment.Id.t * Pool_location.t)
  | Canceled of t
  | Closed of t
  | Deleted of t
  | Updated of (base * Pool_location.t * t)
  | ReminderSent of t
  | Rescheduled of (t * reschedule)
[@@deriving eq, show]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created (session, parent_session_id, experiment_id, location) ->
    let sess =
      create
        ?follow_up_to:parent_session_id
        session.start
        session.duration
        session.description
        location
        session.max_participants
        session.min_participants
        session.overbook
        session.reminder_lead_time
    in
    let%lwt () = Repo.insert pool (Experiment.Id.value experiment_id, sess) in
    Entity_guard.Target.to_authorizable ~ctx:(Pool_tenant.to_ctx pool) sess
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Mailing ] Guard.AuthorizableTarget.t) -> ()
  | Canceled session ->
    (* TODO: Check timestamps? Issue #126 *)
    { session with canceled_at = Some (Ptime_clock.now ()) } |> Repo.update pool
  | Closed session ->
    (* TODO: Check timestamps? Issue #126 *)
    { session with closed_at = Some (Ptime_clock.now ()) } |> Repo.update pool
  | Deleted session -> Repo.delete pool session.id
  | Updated
      ( { start
        ; duration
        ; description
        ; max_participants
        ; min_participants
        ; overbook
        ; reminder_lead_time
        }
      , location
      , session ) ->
    Repo.update
      pool
      { session with
        start
      ; duration
      ; location
      ; description
      ; max_participants
      ; min_participants
      ; overbook
      ; reminder_lead_time
      }
  | ReminderSent session ->
    { session with
      reminder_sent_at = Some (Pool_common.Reminder.SentAt.create_now ())
    }
    |> Repo.update pool
  | Rescheduled (session, { start; duration }) ->
    { session with start; duration } |> Repo.update pool
;;
