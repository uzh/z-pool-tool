open Database

let get_exn = Test_utils.get_or_failwith

module Testable = struct
  let label = Database.Label.(Alcotest.testable pp equal)
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
  let (_ : status) = fetch_pool root in
  Lwt.return_unit
;;

let check_find_tenant_database _ () =
  let expected = [ fst Data.database ] in
  let%lwt tenants = Database.Tenant.find_all_by_status () in
  Alcotest.(check (list Testable.label) "databases found" expected tenants)
  |> Lwt.return
;;

let check_tenant_database _ () =
  let ctx = Data.database_label |> Database.to_ctx in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;
