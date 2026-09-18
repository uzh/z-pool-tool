open Alcotest_lwt

let db_ctx = Test_utils.Data.db_ctx

let case =
  Test_utils.case ~preparation:(fun () ->
    (* let%lwt () = Database.clean_all db_ctx in *)
    Lwt.return_ok ())
;;

let create_and_read_token =
  case
  @@ fun () ->
  let%lwt token = Pool_token.create db_ctx [ "foo", "bar"; "fooz", "baz" ] in
  let%lwt value = Pool_token.read db_ctx token ~k:"foo" in
  Alcotest.(check (option string) "reads value" (Some "bar") value);
  let%lwt is_valid_signature = Pool_token.verify db_ctx token in
  Alcotest.(check bool "has valid signature" true is_valid_signature);
  let%lwt is_active = Pool_token.is_active db_ctx token in
  Alcotest.(check bool "is active" true is_active);
  let%lwt is_expired = Pool_token.is_expired db_ctx token in
  Alcotest.(check bool "is not expired" false is_expired);
  let%lwt is_valid = Pool_token.is_valid db_ctx token in
  Alcotest.(check bool "is valid" true is_valid);
  Lwt.return_ok ()
;;

let deactivate_and_reactivate_token =
  case
  @@ fun () ->
  let%lwt token = Pool_token.create db_ctx [ "foo", "bar" ] in
  let%lwt value = Pool_token.read db_ctx token ~k:"foo" in
  Alcotest.(check (option string) "reads value" (Some "bar") value);
  let%lwt () = Pool_token.deactivate db_ctx token in
  let%lwt value = Pool_token.read db_ctx token ~k:"foo" in
  Alcotest.(check (option string) "reads no value" None value);
  let%lwt value = Pool_token.read db_ctx ~force:() token ~k:"foo" in
  Alcotest.(check (option string) "force reads value" (Some "bar") value);
  let%lwt () = Pool_token.activate db_ctx token in
  let%lwt value = Pool_token.read db_ctx token ~k:"foo" in
  Alcotest.(check (option string) "reads value again" (Some "bar") value);
  Lwt.return_ok ()
;;

let forge_token =
  case
  @@ fun () ->
  let%lwt token = Pool_token.create db_ctx [ "foo", "bar" ] in
  let%lwt value = Pool_token.read db_ctx token ~k:"foo" in
  Alcotest.(check (option string) "reads value" (Some "bar") value);
  let forged_token = "prefix" ^ Pool_token.value token in
  let%lwt value =
    Pool_token.read db_ctx (Pool_token.of_string forged_token) ~k:"foo"
  in
  Alcotest.(check (option string) "reads no value" None value);
  let%lwt value =
    Pool_token.read db_ctx ~force:() (Pool_token.of_string forged_token) ~k:"foo"
  in
  Alcotest.(check (option string) "force doesn't read value" None value);
  let%lwt is_valid_signature =
    Pool_token.verify db_ctx (Pool_token.of_string forged_token)
  in
  Alcotest.(check bool "signature is not valid" false is_valid_signature);
  Lwt.return_ok ()
;;

let extend_expiry_of_expired_token =
  case
  @@ fun () ->
  let%lwt token =
    Pool_token.create ~expires_in:Sihl.Time.OneSecond db_ctx [ "foo", "expired" ]
  in
  let%lwt () = Lwt_unix.sleep 1.1 in
  let%lwt () = Pool_token.extend_expiry db_ctx token Sihl.Time.OneYear in
  let%lwt is_valid = Pool_token.is_valid db_ctx token in
  Alcotest.(check bool "expired token is not extended" false is_valid);
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
      ; test_case "does not extend expired token" `Quick extend_expiry_of_expired_token
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
