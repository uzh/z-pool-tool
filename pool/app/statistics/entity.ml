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

module Pool = struct
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
  [@@deriving eq, show]
end

module ExperimentInvitations = struct
  type t =
    { invitation_resets : Experiment.InvitationReset.t list
    ; sent_since_last_reset : int
    ; total_match_filter : int
    }
  [@@deriving eq, show]
end

module ExperimentFilter = struct
  type t =
    { invited_contacts_count : int
    ; total_match_filter : int
    ; total_uninvited_matching : int
    ; assigned_contacts_not_matching : int
    ; sent_invitations : ExperimentInvitations.t
    }
  [@@deriving eq, show]
end

module ExperimentOverview = struct
  module Field = Pool_message.Field
  module Model = Pool_model.Base

  module RegistrationPossible = struct
    include Model.Boolean

    let field = Field.RegistrationPossible
    let schema = schema field
    let hint = Pool_common.I18n.ExperimentStatisticsRegistrationPossible
  end

  module SessionCount = struct
    include Model.Integer

    let field = Field.SessionCount
    let create = CCResult.return
    let schema = schema field create
  end

  module ShowUpCount = struct
    include Model.Integer

    let field = Field.ShowUpCount
    let create = CCResult.return
    let schema = schema field create
  end

  module NoShowCount = struct
    include Model.Integer

    let field = Field.NoShowCount
    let create = CCResult.return
    let schema = schema field create
  end

  module ParticipationCount = struct
    include Model.Integer

    let field = Field.ParticipantCount
    let create = CCResult.return
    let schema = schema field create
  end

  type t =
    { registration_possible : RegistrationPossible.t
    ; sending_invitations : Experiment.SendingInvitations.t
    ; session_count : SessionCount.t
    ; invitations : ExperimentInvitations.t
    ; show_up_count : ShowUpCount.t
    ; no_show_count : NoShowCount.t
    ; participation_count : ParticipationCount.t
    }
  [@@deriving eq, show, fields ~getters]
end
