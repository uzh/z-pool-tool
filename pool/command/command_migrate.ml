open Database

let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let (_ : status) = Root.add () in
    let%lwt () = Migration.execute root (Pool_migration.Root.steps ()) in
    Lwt.return_some ())
;;

let tenants =
  Command_utils.make_no_args
    "migrate.tenant"
    "Migrate tenant databases"
    (fun () ->
       let (_ : status) = Root.add () in
       let%lwt db_pools = Tenant.setup () in
       let%lwt () =
         Lwt_list.iter_s
           (CCFun.flip Migration.execute (Pool_migration.Tenant.steps ()))
           db_pools
       in
       Lwt.return_some ())
;;
