module Actor = struct
  type t =
    [ `Admin
    | `Assistant of Guardian.Uuid.Target.t
    | `Contact
    | `Experimenter of Guardian.Uuid.Target.t
    | `Guest
    | `LocationManagerAll
    | `LocationManager of Guardian.Uuid.Target.t
    | `OperatorAll
    | `Operator of Guardian.Uuid.Target.t
    | `RecruiterAll
    | `Recruiter of Guardian.Uuid.Target.t
    | `Root (* '`Root' not exposed in 'all' *)
    | `System
    ]
  [@@deriving show, eq, ord, yojson]

  let name t = show t |> Guardian.Util.decompose_variant_string |> fst

  let find_target : t -> Guardian.Uuid.Target.t option = function
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

  let of_string s =
    match Guardian.Util.decompose_variant_string s with
    | "admin", [] -> `Admin
    | "assistant", [ id ] -> `Assistant (Guardian.Uuid.Target.of_string_exn id)
    | "contact", [] -> `Contact
    | "experimenter", [ id ] ->
      `Experimenter (Guardian.Uuid.Target.of_string_exn id)
    | "guest", [] -> `Guest
    | "locationmanagerall", [] -> `LocationManagerAll
    | "locationmanager", [ id ] ->
      `LocationManager (Guardian.Uuid.Target.of_string_exn id)
    | "operatorall", [] -> `OperatorAll
    | "operator", [ id ] -> `Operator (Guardian.Uuid.Target.of_string_exn id)
    | "recruiterall", [] -> `RecruiterAll
    | "recruiter", [ id ] -> `Recruiter (Guardian.Uuid.Target.of_string_exn id)
    | "root", [] -> `Root
    | "system", [] -> `System
    | _ -> failwith ("Invalid role: " ^ s)
  ;;

  let all =
    [ `Assistant Guardian.Uuid.Target.nil
    ; `Contact
    ; `Experimenter Guardian.Uuid.Target.nil
    ; `Guest
    ; `LocationManagerAll
    ; `LocationManager Guardian.Uuid.Target.nil
    ; `OperatorAll
    ; `Operator Guardian.Uuid.Target.nil
    ; `RecruiterAll
    ; `Recruiter Guardian.Uuid.Target.nil
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
    | `Assignment
    | `AssignmentId of Guardian.Uuid.Target.t
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
    | `Session
    | `Setting
    | `Smtp
    | `System
    | `Tenant
    | `WaitingList
    ]
  [@@deriving show, eq, ord, yojson]

  let name t = show t |> Guardian.Util.decompose_variant_string |> fst
  let find_target (_ : t) = None

  let find_target_exn (t : t) =
    find_target t
    |> CCOption.get_exn_or
       @@ Format.asprintf "Cannot get target from role %a" pp t
  ;;

  let of_string s =
    match Guardian.Util.decompose_variant_string s with
    | "admin", [ admin ] ->
      `Admin
        (match admin with
         | "operator" -> `Operator
         | "locationmanager" -> `LocationManager
         | "recruiter" -> `Recruiter
         | "experimenter" -> `Experimenter
         | "assistant" -> `Assistant
         | _ -> failwith ("Invalid role: " ^ s))
    | "assignment", [] -> `Assignment
    | "assignmentid", [ id ] ->
      `AssignmentId (Guardian.Uuid.Target.of_string_exn id)
    | "contact", [] -> `Contact
    | "customfield", [] -> `CustomField
    | "experiment", [] -> `Experiment
    | "filter", [] -> `Filter
    | "i18n", [] -> `I18n
    | "invitation", [] -> `Invitation
    | "location", [] -> `Location
    | "locationfile", [] -> `LocationFile
    | "mailing", [] -> `Mailing
    | "session", [] -> `Session
    | "setting", [] -> `Setting
    | "smtp", [] -> `Smtp
    | "system", [] -> `System
    | "tenant", [] -> `Tenant
    | "waitinglist", [] -> `WaitingList
    | _ -> failwith ("Invalid role: " ^ s)
  ;;

  let all_entities =
    CCList.map
      (fun m -> `Admin m)
      [ `Operator; `LocationManager; `Recruiter; `Experimenter; `Assistant ]
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
      ; `Session
      ; `Setting
      ; `Smtp
      ; `System
      ; `Tenant
      ; `WaitingList
      ]
  ;;

  let all =
    CCList.map
      (fun m -> `Admin m)
      [ `Operator; `LocationManager; `Recruiter; `Experimenter; `Assistant ]
    @ [ `Assignment
      ; `AssignmentId Guardian.Uuid.Target.nil
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
      ; `Session
      ; `Setting
      ; `Smtp
      ; `System
      ; `Tenant
      ; `WaitingList
      ]
  ;;
end
