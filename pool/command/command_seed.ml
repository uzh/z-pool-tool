let root_data =
  Sihl.Command.make
    ~name:"seed.root"
    ~description:"Seed development data to root database"
    (fun _ ->
    Database.Root.setup ();
    let%lwt () = Database.Root.Seed.create () in
    Lwt.return_some ())
;;

let root_data_clean =
  Sihl.Command.make
    ~name:"seed.root.clean"
    ~description:"Clean database and seed development data to root database"
    (fun _ ->
    Database.Root.setup ();
    let%lwt () = Utils.Database.clean_all Database.Root.label in
    let%lwt () = Database.Root.Seed.create () in
    Lwt.return_some ())
;;

let setup_tenants () =
  Database.Root.setup ();
  Database.Tenant.setup ()
;;

let tenant_data =
  Sihl.Command.make
    ~name:"seed.tenant"
    ~description:"Seed development data to tenant databases"
    (fun _ ->
    let%lwt db_pools = setup_tenants () in
    let%lwt () = Database.Tenant.Seed.create db_pools () in
    Lwt.return_some ())
;;

let tenant_data_clean =
  Sihl.Command.make
    ~name:"seed.tenant.clean"
    ~description:"Clean database and seed development data to tenant database"
    (fun _ ->
    let%lwt db_pools = setup_tenants () in
    let%lwt () =
      Lwt_list.iter_s
        (fun pool ->
          let%lwt () =
            Utils.Database.clean_all (Pool_database.Label.value pool)
          in
          Lwt.return_unit)
        db_pools
    in
    let%lwt () = Database.Tenant.Seed.create db_pools () in
    Lwt.return_some ())
;;

let tenant_seed_default =
  Sihl.Command.make
    ~name:"seed.tenant.default"
    ~description:"Seed default tables (without clean)"
    ~help:"<Pool_database>"
    (let help_text =
       {|Provide all fields to seed the default values for a specific tenant:
    <Pool_database>       : string

Example: sihl seed.tenant.default econ-uzh
          |}
     in
     function
     | [ db_pool ] ->
       let () = Database.Root.setup () in
       let%lwt pools = Database.Tenant.setup () in
       let pool =
         let open CCResult in
         Pool_database.Label.create db_pool
         >>= (fun pool ->
               if CCList.exists (Pool_database.Label.equal pool) pools
               then Ok pool
               else Error Pool_common.Message.(NotFound Field.Tenant))
         |> map_err Pool_common.(Utils.error_to_string Language.En)
         |> get_or_failwith
       in
       let%lwt () =
         Pool_event.handle_events
           pool
           [ Settings.(DefaultRestored default_values) |> Pool_event.settings
           ; I18n.(DefaultRestored default_values) |> Pool_event.i18n
           ; Email.(DefaultRestored default_values_tenant |> Pool_event.email)
           ; Guard.(DefaultRestored root_permissions) |> Pool_event.guard
           ]
       in
       Lwt.return_some ()
     | _ ->
       print_endline help_text;
       failwith "Argument missmatch")
;;
