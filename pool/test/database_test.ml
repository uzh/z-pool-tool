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

(* Guards the storage invariant: sessions are pinned to UTC on connect, so
   NOW() and CURRENT_TIMESTAMP defaults agree with UTC-encoded Ptime values. *)
let check_session_time_zone_utc _ () =
  let open Caqti_request.Infix in
  let session_offset_request =
    "SELECT TIMESTAMPDIFF(SECOND, UTC_TIMESTAMP(), NOW())" |> Caqti_type.(unit ->! int)
  in
  let clock_drift_request =
    "SELECT ABS(TIMESTAMPDIFF(SECOND, NOW(), ?))" |> Caqti_type.(ptime ->! int)
  in
  let check label =
    let name suffix = Format.asprintf "%s: %s" (Label.value label) suffix in
    let%lwt session_offset = Database.find label session_offset_request () in
    let%lwt clock_drift = Database.find label clock_drift_request (Ptime_clock.now ()) in
    Alcotest.(check int (name "session time_zone is UTC") 0 session_offset);
    Alcotest.(
      check bool (name "DB clock agrees with Ptime_clock") true (clock_drift <= 2))
    |> Lwt.return
  in
  let%lwt () = check Pool.Root.label in
  check Data.database_label
;;

let select_one_request =
  let open Caqti_request.Infix in
  "SELECT 1" |> Caqti_type.(unit ->! int)
;;

(* A query error is no connection problem: the pool must be kept instead of
   being disconnected and reconnected (churning all of its connections) *)
let check_query_error_keeps_pool _ () =
  let open Caqti_request.Infix in
  let failing_request =
    "SELECT 1 FROM table_that_does_not_exist" |> Caqti_type.(unit ->! int)
  in
  let label = Data.database_label in
  let%lwt pool = Pool.fetch label in
  let%lwt failed =
    Lwt.catch
      (fun () ->
         Database.find label failing_request () |> Lwt.map (fun (_ : int) -> false))
      (function
        | Caqti_error.Exn _ -> Lwt.return_true
        | exn -> Lwt.reraise exn)
  in
  let%lwt pool_after_error = Pool.fetch label in
  let%lwt (_ : int) = Database.find label select_one_request () in
  Alcotest.(check bool "query failed" true failed);
  Alcotest.(check bool "pool kept" true (pool == pool_after_error)) |> Lwt.return
;;

(* Clearing the pool cache (event [TenantDatabaseCacheCleared]) must close the
   connections of the dropped pools instead of leaving them open *)
let check_clear_drains_pools _ () =
  let label = Data.database_label in
  let%lwt (_ : int) = Database.find label select_one_request () in
  let%lwt pool = Pool.fetch label in
  let%lwt () = Pool.initialize ~clear:true () in
  let rec wait_until_drained attempts =
    if Caqti_lwt_unix.Pool.size pool = 0 || attempts = 0
    then Lwt.return_unit
    else Lwt.bind (Lwt_unix.sleep 0.05) (fun () -> wait_until_drained (attempts - 1))
  in
  let%lwt () = wait_until_drained 100 in
  let%lwt new_pool = Pool.fetch label in
  let%lwt (_ : int) = Database.find label select_one_request () in
  Alcotest.(check int "old pool drained" 0 (Caqti_lwt_unix.Pool.size pool));
  Alcotest.(check bool "new pool in use" true (pool != new_pool)) |> Lwt.return
;;
