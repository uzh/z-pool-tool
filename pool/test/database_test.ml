open Pool_database

let fail_with = Test_utils.get_or_failwith_pool_error

module Testable = struct
  let database = Pool_database.(Alcotest.testable pp equal)
end

module Data = struct
  let database_label = "econ-test" |> Label.create |> fail_with

  let database =
    let url =
      Sihl.Configuration.read_string "DATABASE_URL_TENANT_TEST"
      |> CCOption.get_exn_or "DATABASE_URL_TENANT_TEST undefined"
      |> Url.create
      |> fail_with
    in
    database_label, url
  ;;
end

let check_root_database _ () =
  let ctx =
    Database.Root.label |> Pool_database.Label.of_string |> Pool_database.to_ctx
  in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;

let check_find_tenant_database _ () =
  let create label url =
    Pool_database.create label url |> Test_utils.get_or_failwith_pool_error
  in
  let expected = CCList.map (CCFun.uncurry create) [ Data.database ] in
  let%lwt tenants = Pool_tenant.find_databases () in
  Alcotest.(check (list Testable.database) "databases found" expected tenants)
  |> Lwt.return
;;

let check_tenant_database _ () =
  let ctx = Data.database_label |> Pool_database.to_ctx in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;

let update_database _ () =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Pool_tenant_command.UpdateDatabase in
  let label, url = Data.database in
  let database =
    Pool_database.create label url |> Test_utils.get_or_failwith_pool_error
  in
  let%lwt tenant =
    Pool_tenant.find_by_label label
    >== CCFun.flip Pool_tenant.to_write database
    ||> Test_utils.get_or_failwith_pool_error
  in
  let%lwt () =
    handle tenant database
    |> Test_utils.get_or_failwith_pool_error
    |> Pool_event.handle_events Test_utils.Data.database_label
  in
  let%lwt tenants = Pool_tenant.find_databases () in
  let () =
    Alcotest.(
      check
        (list Testable.database)
        "updated database found"
        [ database ]
        tenants)
  in
  Lwt.return_unit
;;
