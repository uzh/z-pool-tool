Printexc.record_backtrace true

let suite =
  Alcotest_lwt.
    [ ( "database"
      , Database_test.
          [ test_case "access root" `Quick check_root_database
          ; test_case "find tenants" `Quick check_find_tenant_database
          ; test_case "access tenant" `Quick check_tenant_database
          ] )
    ; ( "settings"
      , Tenant_settings_test.
          [ test_case "read contact email" `Quick check_contact_email
          ; test_case "has email suffixes" `Quick check_email_suffix
          ; test_case
              "read inactive user disable after"
              `Quick
              check_inactive_user_disable_after
          ; test_case
              "read inactive user warning after"
              `Quick
              check_inactive_user_warning
          ; test_case "read languages" `Quick check_languages
          ; test_case
              "has terms and conditions"
              `Quick
              check_terms_and_conditions
          ; test_case
              "update terms and conditions"
              `Quick
              update_terms_and_conditions
          ; test_case "login after terms update" `Quick login_after_terms_update
          ] )
    ; ( "dev/test"
      , [ test_case "intercept email" `Quick Common_test.validate_email ] )
    ; ( "authorization"
      , Authorization_test.
          [ test_case "permit valid operation" `Quick admin_can_update_language
          ; test_case
              "deny invalid operation"
              `Quick
              guest_cannot_update_language
          ; test_case "use parametric roles" `Quick operator_works
          ] )
    ]
;;

let services =
  [ Database.register ()
  ; Service.User.register ()
  ; Service.Token.register ()
  ; Service.BlockingEmail.register ()
  ; Service.Email.register ()
  ; Service.EmailTemplate.register ()
  ; Service.Queue.register ()
  ; Service.Storage.register ()
  ]
;;

let () =
  Lwt_main.run
    (let%lwt () = Test_utils.setup_test () in
     let%lwt _ = Sihl.Container.start_services services in
     Alcotest_lwt.run "integration" @@ suite)
;;
