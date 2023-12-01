let ( let@ ) = Result.bind
let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f
let ( let& ) = Lwt_result.bind
let test_db = Test_utils.Data.database_label

let services =
  [ Service.User.register ()
  ; Service.Token.register ()
  ; Email.Service.register ()
  ; Email.Service.Queue.register ()
  ; Service.Storage.register ()
  ]
;;

let database_case
  : string -> (unit -> (unit, Pool_common.Message.error) result Lwt.t) -> unit
  =
  fun name test_fn ->
  let open Test_utils in
  let open Alcotest_lwt in
  Printexc.record_backtrace true;
  let test_fn (_switch : Lwt_switch.t) () =
    Sihl.Database.transaction (fun (module _ : Caqti_lwt.CONNECTION) ->
      (* let& () =  Conn.start() in *)
      let result = test_fn () |> Lwt_result.map_error (fun err -> `Test_error err) in
      (* let& () = Conn.rollback () in *)
      result)
    |> Lwt.map (fun result ->
      match result with
      | Error (`Test_error err) -> Pool_common.Utils.get_or_failwith (Error err)
      | Error ((#Caqti_error.load_or_connect | #Caqti_error.transact) as err) ->
        raise (Caqti_error.Exn err)
      | Ok () -> ())
  in
  Lwt_main.run
    (let%lwt db_pools = Command.Utils.setup_databases () in
     let%lwt _ = Command.Seed.seed_tenant_clean ~is_test:true db_pools in
     let%lwt () = setup_test () in
     let%lwt _ = Sihl.Container.start_services services in
     let suite = Alcotest_lwt.[ name, [ test_case "test" `Slow test_fn ] ] in
     run name suite)
;;
