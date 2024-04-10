open CCFun
module TargetId = Guardian.Contract.Uuid.Target

let src = Logs.Src.create "role.entity"

module Actor = struct
  type t =
    [ `Admin
    | `Contact
    | `Guest
    | `System
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> Ok `Admin
    | "contact", [] -> Ok `Contact
    | "guest", [] -> Ok `Guest
    | "system", [] -> Ok `System
    | role -> Error (Guardian.Utils.invalid_role role)
  ;;

  let of_string = of_string_res %> CCResult.get_or_failwith
  let all = [ `Admin; `Contact; `Guest ]
end

module Role = struct
  type t =
    [ `Admin
    | `Assistant
    | `Experimenter
    | `LocationManager
    | `Operator
    | `Recruiter
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> Ok `Admin
    | "assistant", [] -> Ok `Assistant
    | "experimenter", [] -> Ok `Experimenter
    | "locationmanager", [] -> Ok `LocationManager
    | "operator", [] -> Ok `Operator
    | "recruiter", [] -> Ok `Recruiter
    | role -> Error (Guardian.Utils.invalid_role role)
  ;;

  let of_string = of_string_res %> CCResult.get_or_failwith

  let of_name str =
    str
    |> CCString.capitalize_ascii
    |> Format.asprintf "`%s"
    |> of_string_res
    |> CCResult.map_err (CCFun.const Pool_common.Message.(Invalid Field.Target))
  ;;

  let all =
    [ `Admin
    ; `Assistant
    ; `Experimenter
    ; `LocationManager
    ; `Operator
    ; `Recruiter
    ]
  ;;

  let static = [ `Operator ]
  let customizable = CCList.sorted_diff ~cmp:compare all static

  type input_type =
    | QueryExperiments
    | QueryLocations
  [@@deriving show, eq]

  let type_of_key = function
    | `Assistant -> Ok (Some QueryExperiments)
    | `Experimenter -> Ok (Some QueryExperiments)
    | `LocationManager -> Ok (Some QueryLocations)
    | _ -> Ok None
  ;;
end

module Target = struct
  type t =
    [ `Admin
    | `Assignment
    | `Contact
    | `ContactInfo
    | `ContactDirectMessage
    | `ContactName
    | `CustomField
    | `CustomFieldGroup
    | `Experiment
    | `Filter
    | `I18n
    | `Invitation
    | `InvitationNotification
    | `Location
    | `LocationFile
    | `Mailing
    | `Message
    | `MessageTemplate
    | `OrganisationalUnit
    | `Permission
    | `Queue
    | `Role
    | `RoleAdmin
    | `RoleAssistant
    | `RoleExperimenter
    | `RoleLocationManager
    | `RoleOperator
    | `RoleRecruiter
    | `Schedule
    | `Session
    | `SessionClose
    | `Smtp
    | `Statistics
    | `System
    | `SystemSetting
    | `Tag
    | `Tenant
    | `WaitingList
    ]
  [@@deriving show, eq, enum, ord, yojson, sexp_of]

  let name t = show t |> Guardian.Utils.decompose_variant_string |> fst
  let to_admin m = `Admin m

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> Ok `Admin
    | "assignment", [] -> Ok `Assignment
    | "contact", [] -> Ok `Contact
    | "contactinfo", [] -> Ok `ContactInfo
    | "contactdirectmessage", [] -> Ok `ContactDirectMessage
    | "contactname", [] -> Ok `ContactName
    | "customfield", [] -> Ok `CustomField
    | "customfieldgroup", [] -> Ok `CustomFieldGroup
    | "experiment", [] -> Ok `Experiment
    | "filter", [] -> Ok `Filter
    | "i18n", [] -> Ok `I18n
    | "invitation", [] -> Ok `Invitation
    | "invitationnotification", [] -> Ok `InvitationNotification
    | "location", [] -> Ok `Location
    | "locationfile", [] -> Ok `LocationFile
    | "mailing", [] -> Ok `Mailing
    | "message", [] -> Ok `Message
    | "messagetemplate", [] -> Ok `MessageTemplate
    | "organisationalunit", [] -> Ok `OrganisationalUnit
    | "permission", [] -> Ok `Permission
    | "queue", [] -> Ok `Queue
    | "role", [] -> Ok `Role
    | "roleadmin", [] -> Ok `RoleAdmin
    | "roleassistant", [] -> Ok `RoleAssistant
    | "roleexperimenter", [] -> Ok `RoleExperimenter
    | "rolelocationmanager", [] -> Ok `RoleLocationManager
    | "roleoperator", [] -> Ok `RoleOperator
    | "rolerecruiter", [] -> Ok `RoleRecruiter
    | "schedule", [] -> Ok `Schedule
    | "session", [] -> Ok `Session
    | "sessionclose", [] -> Ok `SessionClose
    | "smtp", [] -> Ok `Smtp
    | "statistics", [] -> Ok `Statistics
    | "system", [] -> Ok `System
    | "systemsetting", [] -> Ok `SystemSetting
    | "tag", [] -> Ok `Tag
    | "tenant", [] -> Ok `Tenant
    | "waitinglist", [] -> Ok `WaitingList
    | role -> Error (Guardian.Utils.invalid_role role)
  ;;

  let of_name str =
    str
    |> CCString.capitalize_ascii
    |> Format.asprintf "`%s"
    |> of_string_res
    |> CCResult.map_err (CCFun.const Pool_common.Message.(Invalid Field.Target))
  ;;

  let of_string = of_string_res %> CCResult.get_or_failwith

  let all =
    let open CCList in
    range min max
    |> map of_enum
    |> all_some
    |> CCOption.get_exn_or "Could not create list of all targets!"
  ;;

  let static = [ `Permission; `RoleOperator ]
  let customizable = CCList.sorted_diff ~cmp:compare all static
  let actor_permission = [ `Experiment ]
end
