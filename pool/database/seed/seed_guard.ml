let create pool =
  Guard.(DefaultRestored Guard.all_role_permissions |> handle_event pool)
;;
