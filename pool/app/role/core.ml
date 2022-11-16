module Actor = struct
  type t =
    [ `Admin
    | `Assistant of Guardian.Uuid.Target.t
    | `Contact
    | `Experimenter of Guardian.Uuid.Target.t
    | `Guest
    | `LocationManager of Guardian.Uuid.Target.t
    | `Operator of Guardian.Uuid.Target.t
    | `Recruiter of Guardian.Uuid.Target.t
    | `Student
    | `System
    | `Tenant
    | `User
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
    | "locationmanager", [ id ] ->
      `LocationManager (Guardian.Uuid.Target.of_string_exn id)
    | "operator", [ id ] -> `Operator (Guardian.Uuid.Target.of_string_exn id)
    | "recruiter", [ id ] -> `Recruiter (Guardian.Uuid.Target.of_string_exn id)
    | "student", [] -> `Student
    | "system", [] -> `System
    | "tenant", [] -> `Tenant
    | "user", [] -> `User
    | _ -> failwith ("Invalid role: " ^ s)
  ;;

  let all =
    [ `Admin
    ; `Assistant Guardian.Uuid.Target.nil
    ; `Contact
    ; `Experimenter Guardian.Uuid.Target.nil
    ; `Guest
    ; `LocationManager Guardian.Uuid.Target.nil
    ; `Operator Guardian.Uuid.Target.nil
    ; `Recruiter Guardian.Uuid.Target.nil
    ; `Student
    ; `System
    ; `Tenant
    ; `User
    ]
  ;;
end

module Target = struct
  type t =
    [ `Admin
    | `Assignment
    | `Contact
    | `Experiment
    | `Guest
    | `Mailing
    | `Student
    | `System
    | `Tenant
    | `User
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
    | "admin", [] -> `Admin
    | "assignment", [] -> `Assignment
    | "contact", [] -> `Contact
    | "experiment", [] -> `Experiment
    | "guest", [] -> `Guest
    | "mailing", [] -> `Mailing
    | "student", [] -> `Student
    | "system", [] -> `System
    | "tenant", [] -> `Tenant
    | "user", [] -> `User
    | "waitinglist", [] -> `WaitingList
    | _ -> failwith ("Invalid role: " ^ s)
  ;;

  let all =
    [ `Admin
    ; `Assignment
    ; `Contact
    ; `Experiment
    ; `Guest
    ; `Mailing
    ; `Student
    ; `System
    ; `Tenant
    ; `User
    ; `WaitingList
    ]
  ;;
end
