module Field = Pool_message.Field

module KeyFigure = struct
  include Pool_model.Base.Integer

  let create = CCResult.return
  let schema field = schema field create
  let of_int = CCFun.id
end

module ActiveContacts = struct
  include KeyFigure

  let field = Field.ActiveContactsCount
  let schema = schema field
end

module PendingContactImports = struct
  include KeyFigure

  let field = Field.PendingContactImports
  let schema = schema field
end

module LoginCount = struct
  include KeyFigure

  let field = Field.LoginCount
  let schema = schema field
end

module SignUpCount = struct
  include KeyFigure

  let field = Field.SignUpCount
  let schema = schema field
end

module TermsAcceptedCount = struct
  include KeyFigure

  let field = Field.TermsAcceptedCount
  let schema = schema field
end

module AssignmentsCreated = struct
  include KeyFigure

  let field = Field.AssignmentsCreated
  let schema = schema field
end

module InvitationsSent = struct
  include KeyFigure

  let field = Field.InvitationsSent
  let schema = schema field
end

module RemindersSent = struct
  include KeyFigure

  let field = Field.RemindersSent
  let schema = schema field
end

module EmailsSent = struct
  include KeyFigure

  let field = Field.EmailsSent
  let schema = schema field
end

type period =
  | Min15
  | Hour1
  | Day1
  | Month1
[@@deriving eq, show { with_path = false }, enum, yojson]

let default_period = Min15

let all_periods =
  let open CCList in
  range min_period max_period
  >|= period_of_enum
  |> all_some
  |> CCOption.get_exn_or "Could not create list of all periods!"
;;

let read_period m =
  try Some (Utils.Json.read_variant period_of_yojson m) with
  | _ -> None
;;

let period_to_human_de = function
  | Min15 -> "15 Minuten"
  | Hour1 -> "1 Stunde"
  | Day1 -> "1 Tag"
  | Month1 -> "1 Monat"
;;

let period_to_human_en = function
  | Min15 -> "15 minutes"
  | Hour1 -> "1 hour"
  | Day1 -> "1 day"
  | Month1 -> "1 month"
;;

let period_to_human language t =
  let open Pool_common.Language in
  match language with
  | De -> period_to_human_de t
  | En -> period_to_human_en t
;;

let period_to_sql = function
  | Min15 -> "15 MINUTE"
  | Hour1 -> "1 HOUR"
  | Day1 -> "1 DAY"
  | Month1 -> "1 MONTH"
;;

type t =
  { active_contacts : ActiveContacts.t
  ; pending_contact_imports : PendingContactImports.t
  ; login_count : LoginCount.t
  ; sign_up_count : SignUpCount.t
  ; terms_accepted_count : TermsAcceptedCount.t
  ; terms_last_changed : Pool_model.Base.Ptime.t
  ; assignments_created : AssignmentsCreated.t
  ; invitations_sent : InvitationsSent.t
  ; reminders_sent : RemindersSent.t
  ; emails_sent : EmailsSent.t
  }
[@@deriving eq, show, yojson]

module ExperimentFilter = struct
  type t =
    { contacts_meeting_criteria : int
    ; invitation_count : int
    ; assigned_contacts_not_matching : int
    }

  let create pool experiment query =
    let open Utils.Lwt_result.Infix in
    let open Filter in
    let contacts_not_matching query =
      let%lwt contacts =
        Assignment.find_assigned_contacts_by_experiment pool experiment.Experiment.id
      in
      let%lwt matching =
        Lwt_list.filter_s (Filter.contact_matches_filter pool query) contacts
        ||> CCList.length
      in
      Lwt.return CCList.(length contacts - matching)
    in
    let* contacts_meeting_criteria =
      count_filtered_contacts
        pool
        (Matcher Experiment.(experiment |> id |> Id.to_common))
        query
    in
    let%lwt invitation_count =
      experiment |> Experiment.id |> Experiment.invitation_count pool
    in
    let%lwt assigned_contacts_not_matching =
      query |> CCOption.map_or ~default:(Lwt.return 0) contacts_not_matching
    in
    Lwt_result.return
      { contacts_meeting_criteria; invitation_count; assigned_contacts_not_matching }
  ;;
end
