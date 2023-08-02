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
  try
    Some
      (m
       |> Format.asprintf "[\"%s\"]"
       |> Yojson.Safe.from_string
       |> period_of_yojson)
  with
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
  ; assignments_created : AssignmentsCreated.t
  ; invitations_sent : InvitationsSent.t
  ; sign_up_count : SignUpCount.t
  }
[@@deriving eq, show, yojson]
