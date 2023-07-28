let insert =
  let open Utils.Lwt_result.Infix in
  let help =
    {|<table>

Provide table to insert default data:
        <table>              : string

Available tables:
    - guardian_rules
    - i18n
    - message_templates

Example: seed.default message_templates
    |}
  in
  Sihl.Command.make
    ~name:"seed.default"
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
        >|> Lwt_list.iter_s CCFun.(flip Pool_event.handle_event tenant_event)
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
    | [ "i18n" ] ->
      let events =
        [ I18n.(DefaultRestored default_values) |> Pool_event.i18n ]
      in
      let%lwt () =
        Command_utils.setup_databases ()
        ||> CCList.cons Pool_database.root
        >|> Lwt_list.iter_s CCFun.(flip Pool_event.handle_events events)
      in
      Lwt.return_some ()
    | [ "system_settings" ] ->
      let events =
        [ Settings.(DefaultRestored default_values) |> Pool_event.settings ]
      in
      let%lwt () =
        Command_utils.setup_databases ()
        >|> Lwt_list.iter_s CCFun.(flip Pool_event.handle_events events)
      in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;
