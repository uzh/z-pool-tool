let person = function
  | `Participant -> "participant"
  | `Assistant -> "assistant"
  | `Experimenter -> "experimenter"
  | `LocationManager -> "location_manager"
  | `Recruiter -> "recruiter"
  | `Operator -> "operator"
  | `Root -> "root"
;;

let person_from_string = function
  | "participant" -> `Participant
  | "assistant" -> `Assistant
  | "experimenter" -> `Experimenter
  | "location_manager" -> `LocationManager
  | "recruiter" -> `Recruiter
  | "operator" -> `Operator
  | "root" -> `Root
  | _ -> failwith "Cannot decode role."
;;
