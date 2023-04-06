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
    | `ManageRecruiters
    | `ManageRules
    | `Operator
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

  let equal_or_nil_target (expected : t) (actual : t) : bool =
    equal expected actual
    ||
    match actual with
    | `Assistant _ -> equal expected (`Assistant TargetId.nil)
    | `Experimenter _ -> equal expected (`Experimenter TargetId.nil)
    | `LocationManager _ -> equal expected (`LocationManager TargetId.nil)
    | `ManageAssistant _ -> equal expected (`ManageAssistant TargetId.nil)
    | `ManageExperimenter _ -> equal expected (`ManageExperimenter TargetId.nil)
    | `Recruiter _ -> equal expected (`Recruiter TargetId.nil)
    | _ -> false
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
    ; `System
    ]
  ;;

  let can_assign_roles (role : t) : t list =
    (* let open CCFun.Infix in *)
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
  (* |> CCList.flat_map can_assign_roles *)
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
    | `Queue
    | `Schedule
    | `Session
    | `SystemSetting
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
    | "queue", [] -> `Queue
    | "schedule", [] -> `Schedule
    | "session", [] -> `Session
    | "systemsetting", [] -> `SystemSetting
    | "smtp", [] -> `Smtp
    | "system", [] -> `System
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
    ; `Mailing
    ; `Queue
    ; `Schedule
    ; `Session
    ; `SystemSetting
    ; `Smtp
    ; `System
    ; `Tenant
    ; `WaitingList
    ]
  ;;
end
