let root_data =
  let name = "seed.root" in
  let description = "Seed development data to root database" in
  Command_utils.make_no_args name description (fun () ->
    let%lwt () = Database.Root.setup () in
    let%lwt () = Database.Root.Seed.create () in
    Lwt.return_some ())
;;

let root_data_clean =
  let name = "seed.root.clean" in
  let description =
    "Clean database and seed development data to root database"
  in
  Command_utils.make_no_args name description (fun () ->
    let%lwt () = Database.Root.setup () in
    let%lwt () = Utils.Database.clean_all Database.Root.label in
    let%lwt () = Database.Root.Seed.create () in
    Lwt.return_some ())
;;

let tenant_data =
  let name = "seed.tenant" in
  let description = "Seed development data to tenant databases" in
  Command_utils.make_no_args name description (fun () ->
    let%lwt db_pools = Command_utils.setup_databases () in
    let%lwt () = Database.Tenant.Seed.create db_pools () in
    Lwt.return_some ())
;;

let tenant_data_clean =
  let name = "seed.tenant.clean" in
  let description =
    "Clean database and seed development data to all tenant databases"
  in
  Command_utils.make_no_args name description (fun () ->
    let%lwt db_pools = Command_utils.setup_databases () in
    let%lwt () =
      Lwt_list.iter_s
        CCFun.(Pool_database.Label.value %> Utils.Database.clean_all)
        db_pools
    in
    let%lwt () = Database.Tenant.Seed.create db_pools () in
    Lwt.return_some ())
;;

let tenant_data_clean_specific =
  let name = "seed.tenant.specific.clean" in
  let description =
    "Clean database and seed development data to specific tenant database"
  in
  Command_utils.make_pool_specific name description (fun pool ->
    let%lwt () = Utils.Database.clean_all (Pool_database.Label.value pool) in
    let%lwt () = Database.Tenant.Seed.create [ pool ] () in
    Lwt.return_some ())
;;

let tenant_seed_default =
  Command_utils.make_pool_specific
    "seed.tenant.default"
    "Seed default tables (without clean)"
    (fun pool ->
       let%lwt () =
         [ Settings.(DefaultRestored default_values) |> Pool_event.settings
         ; I18n.(DefaultRestored default_values) |> Pool_event.i18n
         ; Message_template.(
             DefaultRestored default_values_tenant
             |> Pool_event.message_template)
         ; Guard.(DefaultRestored root_permissions) |> Pool_event.guard
         ]
         |> Pool_event.handle_events pool
       in
       Lwt.return_some ())
;;

let tenant_data_contacts_specific =
  let name = "seed.tenant.specific.contacts" in
  let description =
    "Seed 200 additional contacts (development data!) to specific tenant \
     database"
  in
  Command_utils.make_pool_specific name description (fun pool ->
    let%lwt () = Database.Tenant.Seed.create_contacts pool () in
    Lwt.return_some ())
;;
