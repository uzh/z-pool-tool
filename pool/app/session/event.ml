open Entity
module SentAt = Pool_common.Reminder.SentAt

type base =
  { start : Start.t
  ; duration : int
  ; duration_unit : Pool_model.Base.TimeUnit.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : int option
  ; email_reminder_lead_time_unit : Pool_model.Base.TimeUnit.t option
  ; text_message_reminder_lead_time : int option
  ; text_message_reminder_lead_time_unit : Pool_model.Base.TimeUnit.t option
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
  | Updated of (t * t)
  | EmailReminderSent of t
  | TextMsgReminderSent of t
  | Rescheduled of (t * reschedule)
[@@deriving eq, show]

let handle_event ?user_uuid pool =
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  let open Utils.Lwt_result.Infix in
  function
  | Created session ->
    let%lwt () = Repo.insert pool session in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) session.id
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Canceled session ->
    let updated =
      { session with canceled_at = Some (CanceledAt.create_now ()) }
    in
    let%lwt () = create_changelog session updated in
    Repo.update pool updated
  | Closed session ->
    let updated = { session with closed_at = Some (Ptime_clock.now ()) } in
    let%lwt () = create_changelog session updated in
    Repo.update pool updated
  | Deleted session -> Repo.delete pool session.id
  | Updated (session, updated) ->
    let%lwt () = create_changelog session updated in
    Repo.update pool updated
  | EmailReminderSent session ->
    let updated =
      { session with email_reminder_sent_at = Some (SentAt.create_now ()) }
    in
    let%lwt () = create_changelog session updated in
    Repo.update pool updated
  | TextMsgReminderSent session ->
    let updated =
      { session with
        text_message_reminder_sent_at = Some (SentAt.create_now ())
      }
    in
    let%lwt () = create_changelog session updated in
    Repo.update pool updated
  | Rescheduled (session, { start; duration }) ->
    let updated = { session with start; duration } in
    let%lwt () = create_changelog session updated in
    Repo.update pool updated
;;
