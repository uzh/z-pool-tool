open Settings

let create pool =
  let email =
    Database.Label.value pool
    |> Format.asprintf "%s@econ.uzh.ch"
    |> ContactEmail.of_string
  in
  ContactEmailCreated (email, pool) |> handle_event pool
;;
