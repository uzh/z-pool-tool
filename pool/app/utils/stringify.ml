let person = function
  | `Assistant -> "assistant"
  | `Experimenter -> "experimenter"
  | `LocationManager -> "location_manager"
  | `Recruiter -> "recruiter"
  | `Operator -> "operator"
  | `Root -> "root"
;;

let person_from_string = function
  | "assistant" -> `Assistant
  | "experimenter" -> `Experimenter
  | "location_manager" -> `LocationManager
  | "recruiter" -> `Recruiter
  | "operator" -> `Operator
  | "root" -> `Root
  | _ -> failwith "Cannot decode role."
;;
