module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module InternalDescription = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.InternalDescription
  let schema () = schema field ()
end

module PublicDescription = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.PublicDescription
  let schema () = schema field ()
end

(* TODO [aerben] rename to contact *)
module ParticipantAmount = struct
  open Ppx_yojson_conv_lib.Yojson_conv

  type t = int [@@deriving eq, show, yojson]

  let value m = m

  let create amount =
    if amount < 0 then Error Pool_common.Message.(NegativeAmount) else Ok amount
  ;;

  let compare = CCInt.compare

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
  include Pool_common.Model.Ptime

  let create m = m
  let value m = m
  let compare = Ptime.compare

  let schema () =
    let decode str =
      let open CCResult in
      Pool_common.(Utils.Time.parse_time str >|= create)
    in
    Pool_common.(
      Utils.schema_decoder decode Ptime.to_rfc3339 Message.Field.Start)
  ;;
end

module End = struct
  include Pool_common.Model.Ptime

  let create start duration =
    duration
    |> Ptime.add_span start
    |> CCOption.to_result Pool_common.Message.(Invalid Field.Duration)
  ;;

  let value m = m
end

module Duration = struct
  module DurationCore = struct
    let name = Pool_common.Message.Field.Duration
  end

  include Pool_common.Model.Duration (DurationCore)
end

module AssignmentCount = struct
  open Ppx_yojson_conv_lib.Yojson_conv

  type t = int [@@deriving eq, show, yojson]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.AssignmentCount)
    else Ok m
  ;;
end

module NoShowCount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.NoShowCount)
    else Ok m
  ;;
end

module ParticipantCount = struct
  type t = int [@@deriving eq, show]

  let value m = m

  let create m =
    if m < 0
    then Error Pool_common.Message.(Invalid Field.ParticipantCount)
    else Ok m
  ;;
end

module CancellationReason = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Reason

  let validate m =
    if CCString.is_empty m then Error Pool_common.Message.NoValue else Ok m
  ;;

  let schema = schema ?validation:(Some validate) field
end

module CanceledAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Pool_common.Message.Field.CanceledAt create
end

type t =
  { id : Id.t
  ; follow_up_to : Id.t option
  ; has_follow_ups : bool
  ; start : Start.t
  ; duration : Ptime.Span.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; location : Pool_location.t
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; email_reminder_lead_time : Pool_common.Reminder.EmailLeadTime.t option
  ; email_reminder_sent_at : Pool_common.Reminder.SentAt.t option
  ; text_message_reminder_lead_time :
      Pool_common.Reminder.TextMessageLeadTime.t option
  ; text_message_reminder_sent_at : Pool_common.Reminder.SentAt.t option
  ; assignment_count : AssignmentCount.t
  ; no_show_count : NoShowCount.t
  ; participant_count : ParticipantCount.t
  ; (* TODO [aerben] want multiple follow up session?
     * 1. Ja es gibt immer wieder Sessions mit mehreren Following Sessions
     * 2. Eigentlich ist es immer eine Hauptsession mit mehreren Following Sessions

     * Could this model as the following, just flatten tail of linked list
     *  : ; follow_up : t *)
    (* TODO [aerben] make type for canceled_at? *)
    closed_at : Ptime.t option
  ; canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

(* TODO [aerben] need insertion multiple session? *)
(* TODO [aerben] do session copying *)
(* TODO [aerben] write tests *)

let create
  ?id
  ?internal_description
  ?public_description
  ?email_reminder_lead_time
  ?follow_up_to
  ?(has_follow_ups = false)
  ?text_message_reminder_lead_time
  start
  duration
  location
  max_participants
  min_participants
  overbook
  =
  { id = id |> CCOption.value ~default:(Id.create ())
  ; follow_up_to
  ; has_follow_ups
  ; start
  ; duration
  ; internal_description
  ; public_description
  ; location
  ; max_participants
  ; min_participants
  ; overbook
  ; email_reminder_lead_time
  ; email_reminder_sent_at = None
  ; text_message_reminder_lead_time
  ; text_message_reminder_sent_at = None
  ; assignment_count = 0
  ; no_show_count = 0
  ; participant_count = 0
  ; closed_at = None
  ; canceled_at = None
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

let is_canceled_error canceled_at =
  let open Pool_common.Message in
  canceled_at
  |> Pool_common.Utils.Time.formatted_date_time
  |> sessionalreadycanceled
  |> CCResult.fail
;;

let is_fully_booked (m : t) =
  m.assignment_count >= m.max_participants + m.overbook
;;

let is_fully_booked_res (m : t) =
  is_fully_booked m
  |> Utils.bool_to_result_not Pool_common.Message.(SessionFullyBooked)
;;

let available_spots m =
  m.max_participants + m.overbook - m.assignment_count |> CCInt.max 0
;;

let has_assignments m = AssignmentCount.value m.assignment_count > 0

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

let session_date_to_human (session : t) =
  session.start |> Start.value |> Pool_common.Utils.Time.formatted_date_time
;;

let start_end_with_duration_human ({ start; duration; _ } : t) =
  Utils.Ptime.format_start_end_with_duration start duration
;;

let start_end_human ({ start; duration; _ } : t) =
  Utils.Ptime.format_start_end start duration
;;

let compare_start s1 s2 = Start.compare s1.start s2.start

let add_follow_ups_to_parents groups (parent, session) =
  CCList.Assoc.update
    ~eq:Id.equal
    ~f:(fun s ->
      match s with
      | None -> None
      | Some (parent, ls) -> Some (parent, ls @ [ session ]))
    parent
    groups
;;

(* Group follow ups into main sessions and sort by start date *)
let group_and_sort sessions =
  let parents, follow_ups =
    sessions
    |> CCList.partition_filter_map (fun session ->
      match session.follow_up_to with
      | None -> `Left (session.id, (session, []))
      | Some parent -> `Right (parent, session))
  in
  follow_ups
  |> CCList.fold_left add_follow_ups_to_parents parents
  |> CCList.map (fun (_, (p, fs)) -> p, CCList.sort compare_start fs)
  |> CCList.sort (fun (f1, _) (f2, _) -> compare_start f1 f2)
;;

module Public = struct
  type t =
    { id : Id.t
    ; follow_up_to : Id.t option
    ; start : Start.t
    ; duration : Ptime.Span.t
    ; description : PublicDescription.t option
    ; location : Pool_location.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; canceled_at : Ptime.t option
    }
  [@@deriving eq, show]

  let get_session_end (session : t) =
    Ptime.add_span session.start session.duration
    |> CCOption.get_exn_or "Session end not in range"
  ;;

  let not_past session =
    if Ptime.is_later
         (session |> get_session_end |> Start.value)
         ~than:Ptime_clock.(now ())
    then Ok ()
    else Error Pool_common.Message.SessionInPast
  ;;

  let not_canceled (session : t) =
    match session.canceled_at with
    | None -> Ok ()
    | Some canceled_at -> is_canceled_error canceled_at
  ;;

  let is_fully_booked (m : t) =
    m.assignment_count >= m.max_participants + m.overbook
  ;;

  let is_fully_booked_res (m : t) =
    is_fully_booked m
    |> Utils.bool_to_result_not Pool_common.Message.(SessionFullyBooked)
  ;;

  let assignment_creatable session =
    let open CCResult.Infix in
    let* () = is_fully_booked_res session in
    let* () = not_canceled session in
    let* () = not_past session in
    Ok ()
  ;;

  let compare_start (s1 : t) (s2 : t) = Start.compare s1.start s2.start

  let add_follow_ups_and_sort parents =
    let open CCFun in
    CCList.fold_left add_follow_ups_to_parents parents
    %> CCList.map (fun (_, (p, fs)) -> p, CCList.sort compare_start fs)
    %> CCList.sort (fun (f1, _) (f2, _) -> compare_start f1 f2)
  ;;

  (* Group follow ups into main sessions and sort by start date *)
  let group_and_sort sessions =
    let parents, follow_ups =
      sessions
      |> CCList.partition_filter_map (fun (session : t) ->
        match session.follow_up_to with
        | None -> `Left (session.id, (session, []))
        | Some parent -> `Right (parent, session))
    in
    add_follow_ups_and_sort parents follow_ups
  ;;

  let group_and_sort_keep_followups sessions =
    let parents, follow_ups =
      CCList.fold_left
        (fun (parents, follow_ups) (s : t) ->
          let add_parent (s : t) = parents @ [ s.id, (s, []) ], follow_ups in
          match s.follow_up_to with
          | None -> add_parent s
          | Some id
            when CCOption.is_some
                   (CCList.find_opt
                      (fun (parent, _) -> Id.equal parent id)
                      parents) -> parents, follow_ups @ [ id, s ]
          | Some _ -> add_parent s)
        ([], [])
        sessions
    in
    add_follow_ups_and_sort parents follow_ups
  ;;

  let get_session_end (session : t) =
    Ptime.add_span session.start session.duration
    |> CCOption.get_exn_or "Session end not in range"
  ;;

  let start_end_with_duration_human ({ start; duration; _ } : t) =
    Utils.Ptime.format_start_end_with_duration start duration
  ;;
end

let to_public
  ({ id
   ; follow_up_to
   ; start
   ; duration
   ; public_description
   ; location
   ; max_participants
   ; min_participants
   ; overbook
   ; assignment_count
   ; canceled_at
   ; _
   } :
    t)
  =
  Public.
    { id
    ; follow_up_to
    ; start
    ; duration
    ; description = public_description
    ; location
    ; max_participants
    ; min_participants
    ; overbook
    ; assignment_count
    ; canceled_at
    }
;;

module Calendar = struct
  open Ppx_yojson_conv_lib.Yojson_conv

  type contact_person =
    { name : string
    ; email : Pool_user.EmailAddress.t
    }
  [@@deriving eq, show, yojson]

  type location =
    { id : Pool_location.Id.t
    ; name : Pool_location.Name.t
    ; url : string
    }
  [@@deriving eq, show, yojson]

  type links =
    { show_experiment : bool
    ; show_session : bool
    ; show_location_session : bool
    ; experiment : string
    ; session : string
    ; location_session : string
    }
  [@@deriving eq, show, yojson]

  let create_links
    ?(show_experiment = false)
    ?(show_session = false)
    ?(show_location_session = false)
    experiment_id
    session_id
    ({ url; _ } : location)
    =
    let session_id = Id.value session_id in
    let experiment =
      Format.asprintf "admin/experiments/%s" (Experiment.Id.value experiment_id)
      |> Sihl.Web.externalize_path
    in
    let session base_url =
      Format.asprintf "%s/sessions/%s" base_url session_id
    in
    { show_experiment
    ; show_session
    ; show_location_session
    ; experiment
    ; session = session experiment
    ; location_session = session url
    }
  ;;

  type t =
    { id : Id.t
    ; experiment_id : Experiment.Id.t
    ; title : Experiment.Title.t
    ; start : Start.t
    ; end_ : End.t
    ; links : links
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; internal_description : InternalDescription.t option [@option]
    ; location : location
    ; contact_person : contact_person option [@option]
    }
  [@@deriving eq, show, yojson]
end

let email_text language start duration location =
  let format label text =
    Format.asprintf
      "%s: %s"
      (Pool_common.(Utils.field_to_string language label)
       |> CCString.capitalize_ascii)
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

let to_email_text language { start; duration; location; _ } =
  email_text language start duration location
;;

let follow_up_sessions_to_email_list follow_ups =
  follow_ups |> CCList.map start_end_with_duration_human |> CCString.concat "\n"
;;

let public_to_email_text
  language
  (Public.{ start; duration; location; _ } : Public.t)
  =
  email_text language start duration location
;;

let get_session_end session =
  Ptime.add_span session.start session.duration
  |> CCOption.get_exn_or "Session end not in range"
;;

let not_canceled session =
  match session.canceled_at with
  | None -> Ok ()
  | Some canceled_at -> is_canceled_error canceled_at
;;

let not_closed session =
  let open Pool_common.Message in
  match session.closed_at with
  | None -> Ok ()
  | Some closed_at ->
    closed_at
    |> Pool_common.Utils.Time.formatted_date_time
    |> sessionalreadyclosed
    |> CCResult.fail
;;

let not_past session =
  if Ptime.is_later
       (session |> get_session_end |> Start.value)
       ~than:Ptime_clock.(now ())
  then Ok ()
  else Error Pool_common.Message.SessionInPast
;;

(* Cancellable if before session ends *)
let is_cancellable session =
  let open CCResult.Infix in
  let* () = not_canceled session in
  let* () = not_closed session in
  let* () = not_past session in
  Ok ()
;;

let is_deletable session =
  let open CCResult.Infix in
  let* () = not_canceled session in
  let* () = not_closed session in
  match session.has_follow_ups, has_assignments session with
  | true, _ -> Error Pool_common.Message.SessionHasFollowUps
  | _, true -> Error Pool_common.Message.SessionHasAssignments
  | false, false -> Ok ()
;;

let not_closed_or_canceled session =
  let open CCResult.Infix in
  let* () = not_closed session in
  let* () = not_canceled session in
  Ok ()
;;

let is_closable = not_closed_or_canceled
let is_cancelable = not_closed_or_canceled
let assignments_cancelable = not_closed_or_canceled
let assignments_session_changeable = not_closed_or_canceled

let can_be_assigned_to_existing_assignment new_session =
  let open CCResult in
  let* () = is_fully_booked_res new_session in
  let* () = not_closed_or_canceled new_session in
  Ok ()
;;

let assignment_creatable session =
  let open CCResult.Infix in
  let* () = is_fully_booked_res session in
  let* () = not_closed_or_canceled session in
  Ok ()
;;

let reminder_resendable = not_closed_or_canceled

open Pool_common.Message

let column_date = (Field.Date, "pool_sessions.start") |> Query.Column.create

let column_no_assignments =
  (Field.AssignmentCount, "assignment_count") |> Query.Column.create
;;

let column_noshow_count =
  (Field.NoShowCount, "noshow_count") |> Query.Column.create
;;

let column_participation_count =
  (Field.ParticipantCount, "participation_count") |> Query.Column.create
;;

let searchable_by = []

let sortable_by =
  searchable_by
  @ [ column_date
    ; column_no_assignments
    ; column_noshow_count
    ; column_participation_count
    ]
;;

let default_sort =
  let open Query in
  Sort.{ column = column_date; order = SortOrder.Ascending }
;;

let column_canceled =
  Query.Column.create (Field.HideCanceled, "pool_sessions.canceled_at IS NULL")
;;

let column_closed =
  Query.Column.create
    ( Field.HideClosed
    , {sql|
        (pool_sessions.closed_at IS NULL OR EXISTS (
          SELECT TRUE
          FROM pool_sessions AS s
          WHERE pool_sessions.`uuid` = s.follow_up_to AND s.closed_at IS NULL
        ))
      |sql}
    )
;;

let filterable_by =
  Some
    Query.Filter.Condition.Human.
      [ Checkbox column_canceled; Checkbox column_closed ]
;;

let default_filter =
  let open Query in
  let open Filter in
  Condition.[ Checkbox (column_canceled, true); Checkbox (column_closed, true) ]
;;

let default_query = Query.create ~sort:default_sort ~filter:default_filter ()
