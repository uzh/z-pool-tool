let session_id = function
  | `Session { Session.id; _ } -> id
  | `TimeWindow { Time_window.id; _ } -> id
;;

let deletable = function
  | `Session session -> Session.is_deletable session |> CCResult.is_ok
  | `TimeWindow time_window -> Time_window.is_deletable time_window
;;

let canceled_at = function
  | `Session { Session.canceled_at; _ } -> canceled_at
  | `TimeWindow _ -> None
;;

let assignments_cancelable = function
  | `Session session -> Session.assignments_cancelable session |> CCResult.is_ok
  | `TimeWindow _ -> false
;;

let session_changeable session assignment =
  match session with
  | `Session session -> Assignment.session_changeable session assignment |> CCResult.is_ok
  | `TimeWindow _ -> false
;;

let reminder_sendable session assignment =
  match session with
  | `Session session -> Assignment.reminder_sendable session assignment |> CCResult.is_ok
  | `TimeWindow _ -> false
;;

let has_follow_ups = function
  | `Session { Session.has_follow_ups; _ } -> has_follow_ups
  | `TimeWindow _ -> false
;;

let session_title (session : Session.t) =
  Pool_common.I18n.SessionDetailTitle (session.Session.start |> Session.Start.value)
;;

let timewindow_title time_window =
  Pool_common.I18n.TimeWindowDetailTitle
    (Time_window.start_end_with_duration_human time_window)
;;

let detail_page_title = function
  | `Session (session : Session.t) -> session_title session
  | `TimeWindow time_window -> timewindow_title time_window
;;
