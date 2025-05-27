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
  let%lwt (_ : (unit, Pool_message.Error.t) result) =
    Pool.connect Pool.Root.label |> Lwt_result.map_error Pool_common.Utils.with_log_error
  in
  Lwt.return_unit
;;

let check_find_tenant_database _ () =
  let expected = [ fst Data.database ] in
  let tenants = Database.Pool.Tenant.all () in
  Alcotest.(check (list Testable.label) "databases found" expected tenants) |> Lwt.return
;;

let check_tenant_database _ () =
  let ctx = Data.database_label |> Database.to_ctx in
  let _ = Sihl.Database.fetch_pool ~ctx () in
  Lwt.return_unit
;;
