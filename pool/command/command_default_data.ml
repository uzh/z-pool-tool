let insert =
  let open Utils.Lwt_result.Infix in
  let help =
    {|<table>

Provide table to insert default data:
        <table>              : string

Available tables:
    - guardian_role_permission

Example: seed.default guardian_role_permission
    |}
  in
  Sihl.Command.make
    ~name:"seed.default"
    ~description:"Insert required default data into tenant database."
    ~help
    (function
    | [ "guardian_role_permission" ] ->
      let events =
        [ Guard.(DefaultRestored all_role_permissions) |> Pool_event.guard ]
      in
      let%lwt () =
        Command_utils.setup_databases ()
        ||> CCList.cons Pool_database.root
        >|> Lwt_list.iter_s CCFun.(flip Pool_event.handle_events events)
      in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;
