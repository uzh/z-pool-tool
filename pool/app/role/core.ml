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
  | `Assistant of Ocaml_authorize.Uuidm.t
  | `Experimenter of Ocaml_authorize.Uuidm.t
  | `Location_manager of Ocaml_authorize.Uuidm.t
  | `Operator of Ocaml_authorize.Uuidm.t
  | `Recruiter of Ocaml_authorize.Uuidm.t
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

let get_name t = show t |> Ocaml_authorize.Util.decompose_variant_string |> fst

let of_string s =
  match Ocaml_authorize.Util.decompose_variant_string s with
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
  | "assistant", [ id ] -> `Assistant (Ocaml_authorize.Uuidm.of_string_exn id)
  | "experimenter", [ id ] ->
    `Experimenter (Ocaml_authorize.Uuidm.of_string_exn id)
  | "location_manager", [ id ] ->
    `Location_manager (Ocaml_authorize.Uuidm.of_string_exn id)
  | "operator", [ id ] -> `Operator (Ocaml_authorize.Uuidm.of_string_exn id)
  | "recruiter", [ id ] -> `Recruiter (Ocaml_authorize.Uuidm.of_string_exn id)
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
  ; `Assistant Ocaml_authorize.Uuidm.nil
  ; `Experimenter Ocaml_authorize.Uuidm.nil
  ; `Location_manager Ocaml_authorize.Uuidm.nil
  ; `Operator Ocaml_authorize.Uuidm.nil
  ; `Recruiter Ocaml_authorize.Uuidm.nil
  ]
;;
