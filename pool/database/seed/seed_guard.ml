let create pool =
  Guard.(DefaultRestored Guard.root_permissions |> handle_event pool)
;;
