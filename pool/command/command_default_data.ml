let insert =
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
      let%lwt () = Database.Pool.initialize () in
      let%lwt () =
        Database.Pool.all ()
        |> Lwt_list.iter_s CCFun.(flip Pool_event.handle_system_events events)
      in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;
