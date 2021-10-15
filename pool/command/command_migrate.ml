let root =
  Sihl.Command.make
    ~name:"migrate.root"
    ~description:"Migrate root database"
    (fun _ ->
      Database.Root.setup ();
      let%lwt () = Database.Root.Migration.run () in
      Lwt.return_some ())
;;

let tenants =
  Sihl.Command.make
    ~name:"migrate.tenant"
    ~description:"Migrate tenant databases"
    (fun _ ->
      Database.Root.setup ();
      let%lwt db_pools = Database.Tenant.setup () in
      let%lwt () = Database.Tenant.Migration.run db_pools () in
      Lwt.return_some ())
;;
