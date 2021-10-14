Printexc.record_backtrace true

let canary_test () = Alcotest.(check int) "impossible" 4 (2 + 2)

let () =
  let open Alcotest in
  run
    "cqrs commands"
    [ "canary", [ test_case "test suite works" `Quick canary_test ]
    ; ( "participant"
      , [ test_case
            "sign up not allowed suffix"
            `Quick
            Participant_test.sign_up_not_allowed_suffix
        ] )
    ; ( "tenant"
      , [ test_case
            "create tenant smtp auth"
            `Quick
            Tenant_test.create_smtp_auth
        ; test_case "create tenant" `Quick Tenant_test.create_tenant
        ; test_case
            "create tenant with invalid smtp port"
            `Quick
            Tenant_test.create_tenant_invalid_smtp_port
        ; test_case
            "update tenant details"
            `Quick
            Tenant_test.update_tenant_details
        ; test_case
            "update tenant database"
            `Quick
            Tenant_test.update_tenant_database
        ; test_case "create operator" `Quick Tenant_test.create_operator
        ] )
    ; ( "root"
      , [ test_case "create root" `Quick Root_test.create_root
        ; test_case
            "create root with invalid password"
            `Quick
            Root_test.create_root_with_invalid_password
        ] )
    ]
;;
