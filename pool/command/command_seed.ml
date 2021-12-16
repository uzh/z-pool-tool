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
