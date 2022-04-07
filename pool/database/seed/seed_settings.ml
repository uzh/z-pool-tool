let create pool () =
  Settings.(DefaultRestored default_values |> handle_event pool)
;;
