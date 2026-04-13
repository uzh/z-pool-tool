open CCFun
module TargetId = Guardian.Contract.Uuid.Target

let src = Logs.Src.create "role.entity"

module Actor = struct
  type t =
    [ `Admin
    | `ApiKey
    | `Contact
    | `Guest
    | `System
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string_exn %> fst

  let of_string_res input =
    Guardian.Utils.decompose_variant_string input
    |> function
    | Some ("admin", []) -> Ok `Admin
    | Some ("apikey", []) -> Ok `ApiKey
    | Some ("contact", []) -> Ok `Contact
    | Some ("guest", []) -> Ok `Guest
    | Some ("system", []) -> Ok `System
    | Some role -> Error (Guardian.Utils.invalid_role role)
    | None -> Error (Format.asprintf "Invalid actor string: %s" input)
  ;;

  let of_string_exn = of_string_res %> CCResult.get_or_failwith
  let of_string = of_string_res %> CCResult.to_option
  let all = [ `Admin; `Contact; `Guest ]
end

module Role = struct
  type t =
    [ `Assistant
    | `Experimenter
    | `LocationManager
    | `Operator
    | `Recruiter
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string_exn %> fst

  let of_string_res input =
    Guardian.Utils.decompose_variant_string input
    |> function
    | Some ("assistant", []) -> Ok `Assistant
    | Some ("experimenter", []) -> Ok `Experimenter
    | Some ("locationmanager", []) -> Ok `LocationManager
    | Some ("operator", []) -> Ok `Operator
    | Some ("recruiter", []) -> Ok `Recruiter
    | Some role -> Error (Guardian.Utils.invalid_role role)
    | None -> Error (Format.asprintf "Invalid role string: %s" input)
  ;;

  let of_string_exn = of_string_res %> CCResult.get_or_failwith
  let of_string = of_string_res %> CCResult.to_option

  let of_name str =
    str
    |> CCString.capitalize_ascii
    |> Format.asprintf "`%s"
    |> of_string_res
    |> CCResult.map_err (CCFun.const Pool_message.(Error.Invalid Field.Target))
  ;;

  let all = [ `Assistant; `Experimenter; `LocationManager; `Operator; `Recruiter ]
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
    | `Announcement
    | `ApiKey
    | `Assignment
    | `Contact
    | `ContactInfo
    | `ContactDirectMessage
    | `ContactName
    | `CustomField
    | `CustomFieldGroup
    | `DuplicateContact
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
    | `SignupCode
    | `Smtp
    | `Statistics
    | `System
    | `SystemSetting
    | `Tag
    | `Tenant
    | `Version
    | `WaitingList
    ]
  [@@deriving show, eq, enum, ord, yojson, sexp_of]

  let name t = show t |> Guardian.Utils.decompose_variant_string_exn |> fst
  let to_human t = show t |> CCString.take_drop 1 |> snd
  let to_admin m = `Admin m

  let of_string_res input =
    Guardian.Utils.decompose_variant_string input
    |> function
    | Some ("admin", []) -> Ok `Admin
    | Some ("announcement", []) -> Ok `Announcement
    | Some ("apikey", []) -> Ok `ApiKey
    | Some ("assignment", []) -> Ok `Assignment
    | Some ("contact", []) -> Ok `Contact
    | Some ("contactinfo", []) -> Ok `ContactInfo
    | Some ("contactdirectmessage", []) -> Ok `ContactDirectMessage
    | Some ("contactname", []) -> Ok `ContactName
    | Some ("customfield", []) -> Ok `CustomField
    | Some ("customfieldgroup", []) -> Ok `CustomFieldGroup
    | Some ("duplicatecontact", []) -> Ok `DuplicateContact
    | Some ("experiment", []) -> Ok `Experiment
    | Some ("filter", []) -> Ok `Filter
    | Some ("i18n", []) -> Ok `I18n
    | Some ("invitation", []) -> Ok `Invitation
    | Some ("invitationnotification", []) -> Ok `InvitationNotification
    | Some ("location", []) -> Ok `Location
    | Some ("locationfile", []) -> Ok `LocationFile
    | Some ("mailing", []) -> Ok `Mailing
    | Some ("message", []) -> Ok `Message
    | Some ("messagetemplate", []) -> Ok `MessageTemplate
    | Some ("organisationalunit", []) -> Ok `OrganisationalUnit
    | Some ("permission", []) -> Ok `Permission
    | Some ("queue", []) -> Ok `Queue
    | Some ("role", []) -> Ok `Role
    | Some ("roleadmin", []) -> Ok `RoleAdmin
    | Some ("roleassistant", []) -> Ok `RoleAssistant
    | Some ("roleexperimenter", []) -> Ok `RoleExperimenter
    | Some ("rolelocationmanager", []) -> Ok `RoleLocationManager
    | Some ("roleoperator", []) -> Ok `RoleOperator
    | Some ("rolerecruiter", []) -> Ok `RoleRecruiter
    | Some ("schedule", []) -> Ok `Schedule
    | Some ("session", []) -> Ok `Session
    | Some ("sessionclose", []) -> Ok `SessionClose
    | Some ("signupcode", []) -> Ok `SignupCode
    | Some ("smtp", []) -> Ok `Smtp
    | Some ("statistics", []) -> Ok `Statistics
    | Some ("system", []) -> Ok `System
    | Some ("systemsetting", []) -> Ok `SystemSetting
    | Some ("tag", []) -> Ok `Tag
    | Some ("tenant", []) -> Ok `Tenant
    | Some ("version", []) -> Ok `Version
    | Some ("waitinglist", []) -> Ok `WaitingList
    | Some role -> Error (Guardian.Utils.invalid_role role)
    | None -> Error (Format.asprintf "Invalid target string: %s" input)
  ;;

  let of_name str =
    str
    |> CCString.capitalize_ascii
    |> Format.asprintf "`%s"
    |> of_string_res
    |> CCResult.map_err (CCFun.const Pool_message.(Error.Invalid Field.Target))
  ;;

  let of_string_exn = of_string_res %> CCResult.get_or_failwith
  let of_string = of_string_res %> CCResult.to_option

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
