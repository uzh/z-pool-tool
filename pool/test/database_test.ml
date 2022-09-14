module Testable = struct
  let database = Pool_database.(Alcotest.testable pp equal)
end

module Data = struct
  let database_label = "econ-test"

  let database =
    let url =
      Sihl.Configuration.read_string "DATABASE_URL_TENANT_TEST"
      |> CCOption.get_exn_or "DATABASE_URL_TENANT_TEST undefined"
    in
    url, database_label
  ;;
end

let check_root_database _ () =
  let ctx =
    Database.Root.label |> Pool_database.Label.of_string |> Pool_tenant.to_ctx
  in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;

let check_find_tenant_database _ () =
  let create url label =
    Pool_database.create url label |> Test_utils.get_or_failwith_pool_error
  in
  let expected = CCList.map (CCFun.uncurry create) [ Data.database ] in
  let%lwt tenants = Pool_tenant.find_databases () in
  Alcotest.(check (list Testable.database) "databases found" expected tenants)
  |> Lwt.return
;;

let check_tenant_database _ () =
  let ctx =
    Data.database_label |> Pool_database.Label.of_string |> Pool_tenant.to_ctx
  in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;
