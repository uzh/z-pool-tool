let printer = Utils.ppx_printer

open Pool_common
module Field = Message.Field

module RegistrationPossible = struct
  include Model.Boolean

  let field = Field.RegistrationPossible
  let schema = schema field
  let hint = I18n.ExperimentStatisticsRegistrationPossible
end

module SendingInvitations = struct
  module Core = struct
    let field = Pool_common.Message.Field.SendingInvitations

    type t =
      | No [@name "no"] [@printer printer "no"]
      | Sending [@name "sending"] [@printer printer "sending"]
      | Scheduled [@name "scheduld"] [@printer printer "scheduld"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let hint = I18n.ExperimentStatisticsSendingInvitations
end

module SessionCount = struct
  include Model.Integer

  let field = Field.SessionCount
  let create = CCResult.return
  let schema = schema field create
end

module SentInvitationCount = struct
  include Model.Integer

  let field = Field.InvitationCount
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
  ; sending_invitations : SendingInvitations.t
  ; session_count : SessionCount.t
  ; sent_invitation_count : SentInvitationCount.t
  ; showup_count : ShowUpCount.t
  ; noshow_count : NoShowCount.t
  ; participation_count : ParticipationCount.t
  }
[@@deriving eq, show, fields ~getters]
