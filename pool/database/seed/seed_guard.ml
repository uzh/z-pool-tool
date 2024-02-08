open Guard

let create pool =
  DefaultRestored Guard.all_role_permissions |> handle_event pool
;;

let create_role_assignments pool =
  DefaultRestored Guard.all_role_assignment_permissions |> handle_event pool
;;
