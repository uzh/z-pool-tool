let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    Database.Root.setup ();
    let%lwt () = Database.Root.Migration.run () in
    Lwt.return_some ())
;;

let tenants =
  Command_utils.make_no_args
    "migrate.tenant"
    "Migrate tenant databases"
    (fun () ->
    Database.Root.setup ();
    let%lwt db_pools = Database.Tenant.setup () in
    let%lwt () = Database.Tenant.Migration.run db_pools () in
    Lwt.return_some ())
;;
