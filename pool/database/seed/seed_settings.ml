let create pool () =
  let open Settings.Default in
  Settings.DefaultRestored
    ( languages
    , email_suffix
    , contact_email
    , inactive_user_disable_after
    , inactive_user_warning
    , terms_and_conditions )
  |> Settings.handle_event pool
;;
