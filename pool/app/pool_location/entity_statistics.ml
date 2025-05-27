let printer = Utils.ppx_printer

module Field = Pool_message.Field
module Integer = Pool_model.Base.Integer

module ExperimentCount = struct
  include Integer

  let field = Field.ExperimentCount
  let create = CCResult.return
  let schema = schema field create
end

module AssignmentCount = struct
  include Integer

  let field = Field.AssignmentCount
  let create = CCResult.return
  let schema = schema field create
end

module ShowUpCount = struct
  include Integer

  let field = Field.ShowUpCount
  let create = CCResult.return
  let schema = schema field create
end

module NoShowCount = struct
  include Integer

  let field = Field.NoShowCount
  let create = CCResult.return
  let schema = schema field create
end

module ParticipationCount = struct
  include Integer

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
