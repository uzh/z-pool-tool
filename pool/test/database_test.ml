module Testable = struct
  let database = Pool_database.(Alcotest.testable pp equal)
end

module Data = struct
  let database_label = "econ-test"

  let databases =
    let password =
      Sihl.Configuration.read_string "MYSQL_ROOT_PASSWORD"
      |> CCOption.map (Format.asprintf ":%s")
      |> CCOption.get_or ~default:""
    in
    let database =
      Sihl.Configuration.read_string "MYSQL_DATABASE"
      |> CCOption.get_exn_or "MYSQL_DATABASE undefined"
    in
    let url =
      Format.asprintf
        "mariadb://root%s@database-tenant:3306/%s"
        password
        database
    in
    [ url, database_label ]
  ;;
end

let check_root_database _ () =
  let ctx =
    Database.Root.label |> Pool_database.Label.of_string |> Tenant_pool.to_ctx
  in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;

let check_find_tenant_database _ () =
  let create url label =
    Pool_database.create url label |> Test_utils.get_or_failwith_pool_error
  in
  let expected = CCList.map (CCFun.uncurry create) Data.databases in
  let%lwt tenants = Tenant_pool.find_databases () in
  Alcotest.(check (list Testable.database) "databases found" expected tenants)
  |> Lwt.return
;;

let check_tenant_database _ () =
  let ctx =
    Data.database_label |> Pool_database.Label.of_string |> Tenant_pool.to_ctx
  in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;
