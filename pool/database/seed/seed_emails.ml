let tenant pool =
  Email.(DefaultRestored default_values_tenant |> handle_event pool)
;;

let root () =
  Email.(DefaultRestored default_values_root |> handle_event Pool_database.root)
;;
