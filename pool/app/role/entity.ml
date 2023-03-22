open CCFun
module TargetId = Guardian.Contract.Uuid.Target

module Actor = struct
  type t =
    [ `Admin
    | `Assistant of TargetId.t
    | `Contact
    | `Experimenter of TargetId.t
    | `Guest
    | `LocationManagerAll
    | `LocationManager of TargetId.t (* location id*)
    | `ManageAssistant of TargetId.t (* experiment id*)
    | `ManageAssistants
    | `ManageExperimenter of TargetId.t (* experiment id*)
    | `ManageExperimenters
    | `ManageLocationManagers
    | `ManageOperators
    | `ManageOperator of TargetId.t (* tenant id*)
    | `ManageRecruiters
    | `OperatorAll
    | `Operator of TargetId.t
    | `RecruiterAll
    | `Recruiter of TargetId.t
    | `Root (* '`Root' not exposed in 'all' *)
    | `System
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let find_target : t -> TargetId.t option = function
    | `Assistant uuid
    | `Experimenter uuid
    | `LocationManager uuid
    | `ManageAssistant uuid
    | `ManageExperimenter uuid
    | `ManageOperator uuid
    | `Operator uuid
    | `Recruiter uuid -> Some uuid
    | _ -> None
  ;;

  let find_target_exn (t : t) =
    find_target t
    |> CCOption.get_exn_or
       @@ Format.asprintf "Cannot get target from role %a" pp t
  ;;

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
    | "manageoperator", [ id ] -> `ManageOperator (target_of_string id)
    | "manageoperators", [] -> `ManageOperators
    | "managerecruiters", [] -> `ManageRecruiters
    | "operatorall", [] -> `OperatorAll
    | "operator", [ id ] -> `Operator (target_of_string id)
    | "recruiterall", [] -> `RecruiterAll
    | "recruiter", [ id ] -> `Recruiter (target_of_string id)
    | "root", [] -> `Root
    | "system", [] -> `System
    | role -> Guardian.Utils.failwith_invalid_role role
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
    ; `ManageOperator TargetId.nil
    ; `ManageOperators
    ; `ManageRecruiters
    ; `OperatorAll
    ; `Operator TargetId.nil
    ; `RecruiterAll
    ; `Recruiter TargetId.nil
    ; `System
    ]
  ;;
end

module Target = struct
  type t =
    [ `Admin
    | `Assignment
    | `AssignmentId of TargetId.t
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
    | `Queue
    | `Schedule
    | `Session
    | `Setting
    | `Smtp
    | `System
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
    | "assignmentid", [ id ] -> `AssignmentId (TargetId.of_string_exn id)
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
    | "queue", [] -> `Queue
    | "schedule", [] -> `Schedule
    | "session", [] -> `Session
    | "setting", [] -> `Setting
    | "smtp", [] -> `Smtp
    | "system", [] -> `System
    | "tenant", [] -> `Tenant
    | "waitinglist", [] -> `WaitingList
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;

  let all_entities =
    [ `Assignment
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
    ; `Mailing
    ; `Queue
    ; `Schedule
    ; `Session
    ; `Setting
    ; `Smtp
    ; `System
    ; `Tenant
    ; `WaitingList
    ]
  ;;

  let all =
    [ `Assignment
    ; `AssignmentId TargetId.nil
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
    ; `Mailing
    ; `Queue
    ; `Schedule
    ; `Session
    ; `Setting
    ; `Smtp
    ; `System
    ; `Tenant
    ; `WaitingList
    ]
  ;;
end
