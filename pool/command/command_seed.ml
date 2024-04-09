let root_data =
  let name = "seed.root" in
  let description = "Seed development data to root database" in
  Command_utils.make_no_args name description (fun () ->
    let%lwt (_ : Database.status) = Database.Root.setup () in
    let%lwt () = Seed.Root.create () in
    Lwt.return_some ())
;;

let root_data_clean =
  let name = "seed.root.clean" in
  let description =
    "Clean database and seed development data to root database"
  in
  Command_utils.make_no_args name description (fun () ->
    let%lwt (_ : Database.status) = Database.Root.setup () in
    let%lwt () = Database.clean_all_exn Database.root in
    let%lwt () = Seed.Root.create () in
    Lwt.return_some ())
;;

let seed_tenant_clean ?is_test db_pools =
  let%lwt () = Lwt_list.iter_s Database.clean_all_exn db_pools in
  let%lwt () = Seed.Tenant.create ?is_test db_pools () in
  Lwt.return_some ()
;;

let tenant_data =
  let name = "seed.tenant" in
  let description = "Seed development data to tenant databases" in
  Command_utils.make_no_args name description (fun () ->
    let%lwt db_pools = Command_utils.setup_databases () in
    let%lwt () = Seed.Tenant.create db_pools () in
    Lwt.return_some ())
;;

let tenant_data_clean =
  let name = "seed.tenant.clean" in
  let description =
    "Clean database and seed development data to all tenant databases"
  in
  Command_utils.make_no_args name description (fun () ->
    let%lwt db_pools = Command_utils.setup_databases () in
    seed_tenant_clean db_pools)
;;

let tenant_data_test =
  let name = "seed.tenant.test" in
  let description =
    "Clean database and seed development data for test purpose"
  in
  Command_utils.make_no_args name description (fun () ->
    let%lwt db_pools = Command_utils.setup_databases () in
    seed_tenant_clean ~is_test:true db_pools)
;;

let tenant_data_clean_specific =
  let name = "seed.tenant.specific.clean" in
  let description =
    "Clean database and seed development data to specific tenant database"
  in
  Command_utils.make_pool_specific name description (fun pool ->
    seed_tenant_clean [ pool ])
;;

let tenant_data_contacts_specific =
  let name = "seed.tenant.specific.contacts" in
  let description =
    "Seed 200 additional contacts (development data!) to specific tenant \
     database"
  in
  Command_utils.make_pool_specific name description (fun pool ->
    let%lwt () = Seed.Tenant.create_contacts pool () in
    Lwt.return_some ())
;;
