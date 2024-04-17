open Entity

type base =
  { start : Start.t
  ; duration : int
  ; duration_unit : Pool_common.Model.TimeUnit.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : int option
  ; email_reminder_lead_time_unit : Pool_common.Model.TimeUnit.t option
  ; text_message_reminder_lead_time : int option
  ; text_message_reminder_lead_time_unit : Pool_common.Model.TimeUnit.t option
  }
[@@deriving eq, show]

type reschedule =
  { start : Start.t
  ; duration : Duration.t
  }
[@@deriving eq, show]

type event =
  | Created of t
  | Canceled of t
  | Closed of t
  | Deleted of t
  | Updated of t
  | EmailReminderSent of t
  | TextMsgReminderSent of t
  | Rescheduled of (t * reschedule)
[@@deriving eq, show]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created session ->
    let%lwt () = Repo.insert pool session in
    Entity_guard.Target.to_authorizable
      ~ctx:(Pool_database.to_ctx pool)
      session.id
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Canceled session ->
    (* TODO: Check timestamps? Issue #126 *)
    { session with canceled_at = Some (Ptime_clock.now ()) } |> Repo.update pool
  | Closed session ->
    (* TODO: Check timestamps? Issue #126 *)
    { session with closed_at = Some (Ptime_clock.now ()) } |> Repo.update pool
  | Deleted session -> Repo.delete pool session.id
  | Updated session -> Repo.update pool session
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
