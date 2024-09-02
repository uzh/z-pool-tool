open Database

let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let%lwt () = Root.setup () in
    let%lwt () = Migration.execute root (Pool_database.Root.steps ()) in
    Lwt.return_some ())
;;

let tenants =
  Command_utils.make_no_args
    "migrate.tenant"
    "Migrate tenant databases"
    (fun () ->
       let%lwt () = Root.setup () in
       let%lwt db_pools = Tenant.setup () in
       let%lwt () =
         Lwt_list.iter_s
           (CCFun.flip Migration.execute (Pool_database.Tenant.steps ()))
           db_pools
       in
       Lwt.return_some ())
;;
