let printer = Utils.ppx_printer

open Pool_common
module Field = Message.Field

module ExperimentCount = struct
  include Model.Integer

  let field = Field.ExperimentCount
  let create = CCResult.return
  let schema = schema field create
end

module AssignmentCount = struct
  include Model.Integer

  let field = Field.AssignmentCount
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
  { experiment_count : ExperimentCount.t
  ; assignment_count : AssignmentCount.t
  ; showup_count : ShowUpCount.t
  ; noshow_count : NoShowCount.t
  ; participation_count : ParticipationCount.t
  }
[@@deriving eq, show, fields ~getters]
