open Guard

let create pool =
  DefaultRestored Guard.all_role_permissions |> handle_event pool
;;

let operator_can_assign =
  [ `Admin; `Assistant; `Experimenter; `LocationManager; `Operator; `Recruiter ]
;;

let recruiter_can_assign =
  [ `Admin; `Assistant; `Experimenter; `LocationManager ]
;;

let role_assignment pool =
  let assign_per_role role = CCList.map (Guard.RoleAssignment.create role) in
  (operator_can_assign |> assign_per_role `Operator)
  @ (recruiter_can_assign |> assign_per_role `Recruiter)
  |> Guard.Persistence.RoleAssignment.insert pool
;;

let role_assignment_root pool =
  [ Guard.RoleAssignment.create `Operator `Operator ]
  |> Guard.Persistence.RoleAssignment.insert pool
;;
