open Database

let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let (_ : status) = Root.add () in
    let%lwt () = Migration.execute root (Pool_database.Root.steps ()) in
    let () = exit 0 in
    Lwt.return_some ())
;;

let migrate_tenants db_labels =
  let run db_label =
    let set_status = Database.Tenant.update_status db_label in
    Lwt.catch
      (fun () ->
        let%lwt () =
          Migration.execute db_label (Pool_database.Tenant.steps ())
        in
        set_status Status.Active)
      (fun _ ->
        (* TODO: Notify admins *)
        let%lwt () = set_status Status.MigrationsFailed in
        Lwt.return_unit)
  in
  let%lwt () = Database.Tenant.set_migration_pending db_labels in
  db_labels |> Lwt_list.iter_s run
;;

let tenants =
  Command_utils.make_no_args
    "migrate.tenant"
    "Migrate tenant databases"
    (fun () ->
       let (_ : status) = Root.add () in
       let%lwt db_pools = Tenant.setup () in
       let%lwt () = migrate_tenants db_pools in
       Lwt.return_some ())
;;
