open Alcotest_lwt

let database_label = Test_utils.Data.database_label

let case =
  Test_utils.case ~preparation:(fun () ->
    (* let%lwt () = Database.clean_all database_label in *)
    Lwt.return_ok ())
;;

let create_and_read_token =
  case
  @@ fun () ->
  let%lwt token = Pool_token.create database_label [ "foo", "bar"; "fooz", "baz" ] in
  let%lwt value = Pool_token.read database_label token ~k:"foo" in
  Alcotest.(check (option string) "reads value" (Some "bar") value);
  let%lwt is_valid_signature = Pool_token.verify database_label token in
  Alcotest.(check bool "has valid signature" true is_valid_signature);
  let%lwt is_active = Pool_token.is_active database_label token in
  Alcotest.(check bool "is active" true is_active);
  let%lwt is_expired = Pool_token.is_expired database_label token in
  Alcotest.(check bool "is not expired" false is_expired);
  let%lwt is_valid = Pool_token.is_valid database_label token in
  Alcotest.(check bool "is valid" true is_valid);
  Lwt.return_ok ()
;;

let deactivate_and_reactivate_token =
  case
  @@ fun () ->
  let%lwt token = Pool_token.create database_label [ "foo", "bar" ] in
  let%lwt value = Pool_token.read database_label token ~k:"foo" in
  Alcotest.(check (option string) "reads value" (Some "bar") value);
  let%lwt () = Pool_token.deactivate database_label token in
  let%lwt value = Pool_token.read database_label token ~k:"foo" in
  Alcotest.(check (option string) "reads no value" None value);
  let%lwt value = Pool_token.read database_label ~force:() token ~k:"foo" in
  Alcotest.(check (option string) "force reads value" (Some "bar") value);
  let%lwt () = Pool_token.activate database_label token in
  let%lwt value = Pool_token.read database_label token ~k:"foo" in
  Alcotest.(check (option string) "reads value again" (Some "bar") value);
  Lwt.return_ok ()
;;

let forge_token =
  case
  @@ fun () ->
  let%lwt token = Pool_token.create database_label [ "foo", "bar" ] in
  let%lwt value = Pool_token.read database_label token ~k:"foo" in
  Alcotest.(check (option string) "reads value" (Some "bar") value);
  let forged_token = "prefix" ^ token in
  let%lwt value = Pool_token.read database_label forged_token ~k:"foo" in
  Alcotest.(check (option string) "reads no value" None value);
  let%lwt value = Pool_token.read database_label ~force:() forged_token ~k:"foo" in
  Alcotest.(check (option string) "force doesn't read value" None value);
  let%lwt is_valid_signature = Pool_token.verify database_label forged_token in
  Alcotest.(check bool "signature is not valid" false is_valid_signature);
  Lwt.return_ok ()
;;

let suite =
  [ ( "token"
    , [ test_case "create and find token" `Quick create_and_read_token
      ; test_case
          "deactivate and re-activate token"
          `Quick
          deactivate_and_reactivate_token
      ; test_case "forge token" `Quick forge_token
      ] )
  ]
;;

let () =
  let services = [ Pool_database.register (); Pool_token.register () ] in
  Lwt_main.run
    (let%lwt () = Test_utils.setup_test () in
     let%lwt _ = Sihl.Container.start_services services in
     Alcotest_lwt.run "pool_token_test" @@ suite)
;;
