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
    | `LocationManager of TargetId.t
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
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> `Admin
    | "assistant", [ id ] -> `Assistant (TargetId.of_string_exn id)
    | "contact", [] -> `Contact
    | "experimenter", [ id ] -> `Experimenter (TargetId.of_string_exn id)
    | "guest", [] -> `Guest
    | "locationmanagerall", [] -> `LocationManagerAll
    | "locationmanager", [ id ] -> `LocationManager (TargetId.of_string_exn id)
    | "operatorall", [] -> `OperatorAll
    | "operator", [ id ] -> `Operator (TargetId.of_string_exn id)
    | "recruiterall", [] -> `RecruiterAll
    | "recruiter", [ id ] -> `Recruiter (TargetId.of_string_exn id)
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
    ; `OperatorAll
    ; `Operator TargetId.nil
    ; `RecruiterAll
    ; `Recruiter TargetId.nil
    ; `System
    ]
  ;;
end

module Target = struct
  type admins =
    [ `Operator
    | `LocationManager
    | `Recruiter
    | `Experimenter
    | `Assistant
    ]
  [@@deriving show, eq, ord, yojson]

  type t =
    [ `Admin of admins
    | `AdminAny
    | `Assignment
    | `AssignmentId of TargetId.t
    | `Contact
    | `CustomField
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
    | "admin", [ admin ] ->
      `Admin
        (match Guardian.Utils.decompose_variant_string admin with
         | "operator", [] -> `Operator
         | "locationmanager", [] -> `LocationManager
         | "recruiter", [] -> `Recruiter
         | "experimenter", [] -> `Experimenter
         | "assistant", [] -> `Assistant
         | role ->
           Guardian.Utils.failwith_invalid_role
             ~msg_prefix:"Invalid admin role"
             role)
    | "adminany", [] -> `AdminAny
    | "assignment", [] -> `Assignment
    | "assignmentid", [ id ] -> `AssignmentId (TargetId.of_string_exn id)
    | "contact", [] -> `Contact
    | "customfield", [] -> `CustomField
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

  let all_admins =
    CCList.map
      to_admin
      [ `Operator; `LocationManager; `Recruiter; `Experimenter; `Assistant ]
  ;;

  let all_entities =
    all_admins
    @ [ `Assignment
      ; `Contact
      ; `CustomField
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
    all_admins
    @ [ `Assignment
      ; `AssignmentId TargetId.nil
      ; `Contact
      ; `CustomField
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
