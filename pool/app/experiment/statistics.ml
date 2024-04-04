module Field = Pool_common.Message.Field
module Model = Pool_common.Model

module SentInvitations = struct
  type sent_by_count = int * int [@@deriving eq, show]

  type statistics =
    { total_sent : int
    ; total_match_filter : int
    ; sent_by_count : sent_by_count list
    }
  [@@deriving eq, show]
end

module RegistrationPossible = struct
  include Model.Boolean

  let field = Field.RegistrationPossible
  let schema = schema field
  let hint = Pool_common.I18n.ExperimentStatisticsRegistrationPossible
end

module SendingInvitations = struct
  module Core = struct
    let field = Pool_common.Message.Field.SendingInvitations

    type t =
      | No [@name "no"] [@printer Utils.ppx_printer "no"]
      | Sending [@name "sending"] [@printer Utils.ppx_printer "sending"]
      | Scheduled [@name "scheduled"] [@printer Utils.ppx_printer "scheduled"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let read str =
    try Ok (Utils.Json.read_variant t_of_yojson str) with
    | _ -> Error (Pool_common.Message.Invalid field)
  ;;

  let hint = Pool_common.I18n.ExperimentStatisticsSendingInvitations
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

type statistics =
  { registration_possible : RegistrationPossible.t
  ; sending_invitations : SendingInvitations.t
  ; session_count : SessionCount.t
  ; invitations : SentInvitations.statistics
  ; showup_count : ShowUpCount.t
  ; noshow_count : NoShowCount.t
  ; participation_count : ParticipationCount.t
  }
[@@deriving eq, show, fields ~getters]
