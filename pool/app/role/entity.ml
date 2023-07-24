open CCFun
module TargetId = Guardian.Contract.Uuid.Target

let src = Logs.Src.create "role.entity"

module Actor = struct
  type t =
    [ `Admin
    | `Assistant of TargetId.t (* experiment id*)
    | `Contact
    | `Experimenter of TargetId.t (* experiment id*)
    | `Guest
    | `LocationManagerAll
    | `LocationManager of TargetId.t (* location id*)
    | `ManageAssistant of TargetId.t (* experiment id*)
    | `ManageAssistants
    | `ManageExperimenter of TargetId.t (* experiment id*)
    | `ManageExperimenters
    | `ManageLocationManagers
    | `ManageOperators
    | `ManageRecruiters
    | `ManageRules
    | `Operator
    | `RecruiterAll
    | `Recruiter of TargetId.t
    | `Root (* '`Root' not exposed in 'all' *)
    | `System (* '`System' not exposed in 'all' *)
    ]
  [@@deriving show, eq, ord, yojson]

  let key_to_string =
    show
    %> CCString.replace
         ~which:`Right
         ~sub:(Format.asprintf " (%s)" TargetId.(nil |> to_string))
         ~by:""
  ;;

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string =
    let target_of_string = TargetId.of_string_exn in
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> `Admin
    | "assistant", [ id ] -> `Assistant (target_of_string id)
    | "contact", [] -> `Contact
    | "experimenter", [ id ] -> `Experimenter (target_of_string id)
    | "guest", [] -> `Guest
    | "locationmanagerall", [] -> `LocationManagerAll
    | "locationmanager", [ id ] -> `LocationManager (target_of_string id)
    | "manageassistant", [ id ] -> `ManageAssistant (target_of_string id)
    | "manageassistants", [] -> `ManageAssistants
    | "manageexperimenter", [ id ] -> `ManageExperimenter (target_of_string id)
    | "manageexperimenters", [] -> `ManageExperimenters
    | "managelocationmanagers", [] -> `ManageLocationManagers
    | "manageoperators", [] -> `ManageOperators
    | "managerecruiters", [] -> `ManageRecruiters
    | "managerules", [] -> `ManageRules
    | "operator", [] -> `Operator
    | "recruiterall", [] -> `RecruiterAll
    | "recruiter", [ id ] -> `Recruiter (target_of_string id)
    | "root", [] -> `Root
    | "system", [] -> `System
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;

  let of_string_res role =
    try of_string role |> CCResult.return with
    | _ -> Error Pool_common.Message.(NotFound Field.Role)
  ;;

  let find_target : t -> TargetId.t option = function
    | `Assistant uuid
    | `Experimenter uuid
    | `LocationManager uuid
    | `ManageAssistant uuid
    | `ManageExperimenter uuid
    | `Recruiter uuid -> Some uuid
    | _ -> None
  ;;

  let find_target_exn (t : t) =
    find_target t
    |> CCOption.get_exn_or
       @@ Format.asprintf "Cannot get target from role %a" pp t
  ;;

  let has_nil_target =
    let open TargetId in
    function
    | `Assistant uuid
    | `Experimenter uuid
    | `LocationManager uuid
    | `ManageAssistant uuid
    | `ManageExperimenter uuid
    | `Recruiter uuid -> equal uuid nil
    | _ -> false
  ;;

  let update_target role id : t =
    if find_target role |> CCOption.is_some
    then (
      match role with
      | `Assistant _ -> `Assistant id
      | `Experimenter _ -> `Experimenter id
      | `LocationManager _ -> `LocationManager id
      | `ManageAssistant _ -> `ManageAssistant id
      | `ManageExperimenter _ -> `ManageExperimenter id
      | `Recruiter _ -> `Recruiter id
      | role ->
        let msg =
          show
          %> Format.asprintf "Role 'with_target': Missing role with target '%s'"
        in
        Logs.err ~src (fun m -> m "%s" (msg role));
        failwith (msg role))
    else role
  ;;

  let with_nil_target = flip update_target TargetId.nil

  let equal_or_nil_target (expected : t) (actual : t) : bool =
    equal expected actual || equal expected (with_nil_target actual)
  ;;

  let find_target_of expected actual : TargetId.t option =
    if equal_or_nil_target expected actual then find_target actual else None
  ;;

  let all =
    [ `Assistant TargetId.nil
    ; `Contact
    ; `Experimenter TargetId.nil
    ; `Guest
    ; `LocationManagerAll
    ; `LocationManager TargetId.nil
    ; `ManageAssistant TargetId.nil
    ; `ManageAssistants
    ; `ManageExperimenter TargetId.nil
    ; `ManageExperimenters
    ; `ManageLocationManagers
    ; `ManageOperators
    ; `ManageRecruiters
    ; `Operator
    ; `RecruiterAll
    ; `Recruiter TargetId.nil
    ]
  ;;

  let can_assign_roles (role : t) : t list =
    match role with
    | `Admin
    | `Assistant _
    | `Contact
    | `Experimenter _
    | `Guest
    | `LocationManagerAll
    | `LocationManager _ -> []
    | `ManageAssistant uuid -> [ `Assistant uuid ]
    | `ManageAssistants -> [ `Assistant TargetId.nil ]
    | `ManageExperimenter uuid -> [ `Experimenter uuid ]
    | `ManageExperimenters -> [ `Experimenter TargetId.nil ]
    | `ManageLocationManagers ->
      [ `LocationManager TargetId.nil; `LocationManagerAll ]
    | `ManageOperators -> [ `Operator ]
    | `ManageRecruiters -> [ `Recruiter TargetId.nil; `RecruiterAll ]
    | `RecruiterAll -> [ `Assistant TargetId.nil; `Experimenter TargetId.nil ]
    | `Recruiter uuid -> [ `Assistant uuid; `Experimenter uuid ]
    | `ManageRules | `Operator | `Root | `System -> all
  ;;

  type input_type =
    | QueryExperiments
    | QueryLocations
  [@@deriving show, eq]

  let type_of_key = function
    | `Assistant _
    | `Experimenter _
    | `ManageAssistant _
    | `ManageExperimenter _
    | `Recruiter _ -> Ok (Some QueryExperiments)
    | `LocationManager _ -> Ok (Some QueryLocations)
    | elem when find_target elem |> CCOption.is_some ->
      Error
        Pool_common.(
          Message.(NotHandled (Utils.field_to_string Language.En Field.Role)))
    | _ -> Ok None
  ;;
end

module Target = struct
  type t =
    [ `Admin
    | `Assignment
    | `Contact
    | `CustomField
    | `CustomFieldGroup
    | `Experiment
    | `Filter
    | `I18n
    | `Invitation
    | `Location
    | `LocationFile
    | `Mailing
    | `MessageTemplate
    | `OrganisationalUnit
    | `Queue
    | `Schedule
    | `Session
    | `SystemSetting
    | `Smtp
    | `System
    | `Tag
    | `Tenant
    | `WaitingList
    ]
  [@@deriving show, eq, ord, yojson]

  let name t = show t |> Guardian.Utils.decompose_variant_string |> fst
  let find_target (_ : t) = None

  let find_target_exn (t : t) =
    find_target t
    |> CCOption.get_exn_or
       @@ Format.asprintf "Cannot get target from role %a" pp t
  ;;

  let to_admin m = `Admin m

  let of_string =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> `Admin
    | "assignment", [] -> `Assignment
    | "contact", [] -> `Contact
    | "customfield", [] -> `CustomField
    | "customfieldgroup", [] -> `CustomFieldGroup
    | "experiment", [] -> `Experiment
    | "filter", [] -> `Filter
    | "i18n", [] -> `I18n
    | "invitation", [] -> `Invitation
    | "location", [] -> `Location
    | "locationfile", [] -> `LocationFile
    | "mailing", [] -> `Mailing
    | "messagetemplate", [] -> `MessageTemplate
    | "organisationalunit", [] -> `OrganisationalUnit
    | "queue", [] -> `Queue
    | "schedule", [] -> `Schedule
    | "session", [] -> `Session
    | "systemsetting", [] -> `SystemSetting
    | "smtp", [] -> `Smtp
    | "system", [] -> `System
    | "tag", [] -> `Tag
    | "tenant", [] -> `Tenant
    | "waitinglist", [] -> `WaitingList
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;

  let all =
    [ `Admin
    ; `Assignment
    ; `Contact
    ; `CustomField
    ; `CustomFieldGroup
    ; `Experiment
    ; `Filter
    ; `I18n
    ; `Invitation
    ; `Location
    ; `LocationFile
    ; `MessageTemplate
    ; `OrganisationalUnit
    ; `Mailing
    ; `Queue
    ; `Schedule
    ; `Session
    ; `SystemSetting
    ; `Smtp
    ; `System
    ; `Tag
    ; `Tenant
    ; `WaitingList
    ]
  ;;
end
