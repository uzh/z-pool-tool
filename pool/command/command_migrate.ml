let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let (_ : Database.status) = Pool_database.Root.add () in
    let%lwt () = Pool_database.Root.Migration.run () in
    Lwt.return_some ())
;;

let tenants =
  Command_utils.make_no_args
    "migrate.tenant"
    "Migrate tenant databases"
    (fun () ->
       let (_ : Database.status) = Pool_database.Root.add () in
       let%lwt db_pools = Pool_database.Tenant.setup () in
       let%lwt () = Pool_database.Tenant.Migration.run db_pools () in
       Lwt.return_some ())
;;
