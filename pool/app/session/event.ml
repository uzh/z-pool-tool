open Entity

type base =
  { start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; limitations : Limitations.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; text_message_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }
[@@deriving eq, show]

type update =
  { start : Start.t option
  ; duration : Duration.t option
  ; description : Description.t option
  ; limitations : Limitations.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; text_message_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }
[@@deriving eq, show]

type reschedule =
  { start : Start.t
  ; duration : Duration.t
  }
[@@deriving eq, show]

type event =
  | Created of (t * Experiment.Id.t)
  | Canceled of t
  | Closed of t
  | Deleted of t
  | Updated of (base * Pool_location.t * t)
  | EmailReminderSent of t
  | TextMsgReminderSent of t
  | Rescheduled of (t * reschedule)
[@@deriving eq, show]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created (session, experiment_id) ->
    let%lwt () =
      Repo.insert pool (Experiment.Id.value experiment_id, session)
    in
    Entity_guard.Target.to_authorizable ~ctx:(Pool_database.to_ctx pool) session
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Role.Target.t Guard.Target.t) -> ()
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
        ; limitations
        ; max_participants
        ; min_participants
        ; overbook
        ; email_reminder_lead_time
        ; text_message_reminder_lead_time
        }
      , location
      , session ) ->
    Repo.update
      pool
      { session with
        start
      ; duration
      ; limitations
      ; location
      ; description
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time
      ; text_message_reminder_lead_time
      }
  | EmailReminderSent session ->
    { session with
      email_reminder_sent_at = Some (Pool_common.Reminder.SentAt.create_now ())
    }
    |> Repo.update pool
  | TextMsgReminderSent session ->
    { session with
      text_message_reminder_sent_at =
        Some (Pool_common.Reminder.SentAt.create_now ())
    }
    |> Repo.update pool
  | Rescheduled (session, { start; duration }) ->
    { session with start; duration } |> Repo.update pool
;;
