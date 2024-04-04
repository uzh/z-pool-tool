open Database

let get_exn = Test_utils.get_or_failwith

module Testable = struct
  let database = Database.(Alcotest.testable pp equal)
end

module Data = struct
  let database_label = "econ-test" |> Label.create |> get_exn

  let database =
    let url =
      Sihl.Configuration.read_string "DATABASE_URL_TENANT_ONE"
      |> CCOption.get_exn_or "DATABASE_URL_TENANT_ONE undefined"
      |> Url.create
      |> get_exn
    in
    database_label, url
  ;;
end

let check_root_database _ () =
  let ctx = Pool_database.Root.label |> Database.to_ctx in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;

let check_find_tenant_database _ () =
  let expected = CCList.map (CCFun.uncurry Database.create) [ Data.database ] in
  let%lwt tenants = Pool_tenant.find_databases () in
  Alcotest.(check (list Testable.database) "databases found" expected tenants)
  |> Lwt.return
;;

let check_tenant_database _ () =
  let ctx = Data.database_label |> Database.to_ctx in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;
