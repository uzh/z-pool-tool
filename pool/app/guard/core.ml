include Guardian.Make (Role.Actor) (Role.Role) (Role.Target)

module Uuid = struct
  include Uuid

  let actor_of fcn = CCFun.(fcn %> Actor.of_string_exn)
  let target_of fcn = CCFun.(fcn %> Target.of_string_exn)
end

module Utils = struct
  include Utils

  let find_assignable_target_role : Role.Role.t -> Role.Target.t = function
    | `Admin -> `RoleAdmin
    | `Assistant -> `RoleAssistant
    | `Experimenter -> `RoleExperimenter
    | `LocationManager -> `RoleLocationManager
    | `Operator -> `RoleOperator
    | `Recruiter -> `RoleRecruiter
  ;;

  let find_assignable_role
    : Role.Target.t -> (Role.Role.t, Pool_common.Message.error) result
    = function
    | `RoleAdmin -> Ok `Admin
    | `RoleAssistant -> Ok `Assistant
    | `RoleExperimenter -> Ok `Experimenter
    | `RoleLocationManager -> Ok `LocationManager
    | `RoleOperator -> Ok `Operator
    | `RoleRecruiter -> Ok `Recruiter
    | _ -> Error Pool_common.Message.(NotFound Field.Role)
  ;;
end
