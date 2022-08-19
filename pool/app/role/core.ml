type t =
  [ `Admin
  | `Tenant
  | `Guest
  | `Student
  | `Experiment
  | `System
  | `User
  | `Participant
  | `Contact
  | `Waiting_list
  | `Assignment
  | `Mailing
  | `Assistant of Guardian.Uuidm.t
  | `Experimenter of Guardian.Uuidm.t
  | `Location_manager of Guardian.Uuidm.t
  | `Operator of Guardian.Uuidm.t
  | `Recruiter of Guardian.Uuidm.t
  ]
[@@deriving show, eq, ord, yojson]

let get_target (t : t) =
  match t with
  | `Assistant uuid
  | `Experimenter uuid
  | `Location_manager uuid
  | `Operator uuid
  | `Recruiter uuid -> uuid
  | _ -> failwith (Printf.sprintf "Cannot get target from role %s" (show t))
;;

let get_name t = show t |> Guardian.Util.decompose_variant_string |> fst

let of_string s =
  match Guardian.Util.decompose_variant_string s with
  | "admin", [] -> `Admin
  | "tenant", [] -> `Tenant
  | "guest", [] -> `Guest
  | "student", [] -> `Student
  | "experiment", [] -> `Experiment
  | "system", [] -> `System
  | "user", [] -> `User
  | "contact", [] -> `Contact
  | "waiting_list", [] -> `Waiting_list
  | "assignment", [] -> `Assignment
  | "mailing", [] -> `Mailing
  | "assistant", [ id ] -> `Assistant (Guardian.Uuidm.of_string_exn id)
  | "experimenter", [ id ] -> `Experimenter (Guardian.Uuidm.of_string_exn id)
  | "location_manager", [ id ] ->
    `Location_manager (Guardian.Uuidm.of_string_exn id)
  | "operator", [ id ] -> `Operator (Guardian.Uuidm.of_string_exn id)
  | "recruiter", [ id ] -> `Recruiter (Guardian.Uuidm.of_string_exn id)
  | _ -> failwith ("Invalid role: " ^ s)
;;

let all =
  [ `Admin
  ; `Tenant
  ; `Guest
  ; `Student
  ; `Experiment
  ; `System
  ; `User
  ; `Participant
  ; `Contact
  ; `Waiting_list
  ; `Assignment
  ; `Mailing
  ; `Assistant Guardian.Uuidm.nil
  ; `Experimenter Guardian.Uuidm.nil
  ; `Location_manager Guardian.Uuidm.nil
  ; `Operator Guardian.Uuidm.nil
  ; `Recruiter Guardian.Uuidm.nil
  ]
;;
