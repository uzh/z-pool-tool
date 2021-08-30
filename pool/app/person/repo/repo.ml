open Entity
module RepoPerson = Repo_person

let extract : type a. a Entity.carrier -> a Entity.t Caqti_type.t * string =
  let open Repo_person in
  let open Utils.Stringify in
  function
  | ParticipantC -> participant, person `Participant
  | AssistantC -> assistant, person `Assistant
  | ExperimenterC -> experimenter, person `Experimenter
  | LocationManagerC -> location_manager, person `LocationManager
  | RecruiterC -> recruiter, person `Recruiter
  | OperatorC -> operator, person `Operator
;;

let find = Utils.todo
let insert = Utils.todo
let update = Utils.todo

let set_password
    : type person. person t -> string -> (unit, string) result Lwt.t
  =
 fun person password ->
  let open Lwt_result.Infix in
  match person with
  | Participant m
  | Assistant m
  | Experimenter m
  | LocationManager m
  | Recruiter m
  | Operator m ->
    (* TODO add password confirmation *)
    Service.User.set_password m.user ~password ~password_confirmation:password
    >|= CCFun.const ()
;;
