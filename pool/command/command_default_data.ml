let insert =
  let open Utils.Lwt_result.Infix in
  let help =
    {|<table>

Provide table to insert default data:
        <table>              : string

Available tables: message_templates

Example: default_data.insert message_templates
    |}
  in
  Sihl.Command.make
    ~name:"default_data.insert"
    ~description:"Insert required default data into tenant database."
    ~help
    (function
    | [ "message_templates" ] ->
      let root_event =
        Message_template.(DefaultRestored default_values_root)
        |> Pool_event.message_template
      in
      let tenant_event =
        Message_template.(DefaultRestored default_values_tenant)
        |> Pool_event.message_template
      in
      let%lwt () =
        Command_utils.setup_databases ()
        >|> Lwt_list.iter_s (fun pool ->
              (Pool_event.handle_event pool) tenant_event)
      in
      let%lwt () = Pool_event.handle_event Pool_database.root root_event in
      Lwt.return_some ()
    | [ "guardian_rules" ] ->
      let events =
        [ Guard.(DefaultRestored root_permissions) |> Pool_event.guard ]
      in
      let%lwt () =
        Command_utils.setup_databases ()
        ||> CCList.cons Pool_database.root
        >|> Lwt_list.iter_s CCFun.(flip Pool_event.handle_events events)
      in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;
