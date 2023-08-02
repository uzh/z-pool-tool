module Field = Pool_common.Message.Field

module ActiveContacts = struct
  include Pool_common.Model.Integer

  let field = Field.ActiveContactsCount
  let create = CCResult.return
  let schema = schema field create
  let of_int = CCFun.id
end

module PendingContactImports = struct
  include Pool_common.Model.Integer

  let field = Field.PendingContactImports
  let create = CCResult.return
  let schema = schema field create
  let of_int = CCFun.id
end

module AssignmentsCreated = struct
  include Pool_common.Model.Integer

  let field = Field.AssignmentsCreated
  let create = CCResult.return
  let schema = schema field create
  let of_int = CCFun.id
end

module InvitationsSent = struct
  include Pool_common.Model.Integer

  let field = Field.InvitationsSent
  let create = CCResult.return
  let schema = schema field create
  let of_int = CCFun.id
end

module SignUpCount = struct
  include Pool_common.Model.Integer

  let field = Field.SignUpCount
  let create = CCResult.return
  let schema = schema field create
  let of_int = CCFun.id
end

type period =
  | Min15
  | Hour1
  | Day1
  | Month1

let default_period = Min15

let period_to_sql = function
  | Min15 -> "15 MINUTE"
  | Hour1 -> "1 HOUR"
  | Day1 -> "1 DAY"
  | Month1 -> "1 MONTH"
;;

type t =
  { active_contacts : ActiveContacts.t
  ; pending_contact_imports : PendingContactImports.t
  ; assignments_created : AssignmentsCreated.t
  ; invitations_sent : InvitationsSent.t
  ; sign_up_count : SignUpCount.t
  }
[@@deriving eq, show, yojson]
