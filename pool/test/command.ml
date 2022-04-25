Printexc.record_backtrace true

let () =
  let open Alcotest in
  run
    "cqrs commands"
    [ ( "participant"
      , [ test_case
            "sign up not allowed suffix"
            `Quick
            Participant_test.sign_up_not_allowed_suffix
        ; test_case "sign up" `Quick Participant_test.sign_up_not_allowed_suffix
        ] )
    ; ( "tenant"
      , [ test_case
            "create tenant smtp auth"
            `Quick
            Tenant_test.create_smtp_auth
        ; test_case "create tenant" `Quick Tenant_test.create_tenant
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
    ; ( "i18n"
      , [ test_case "create translation" `Quick I18n_test.create
        ; test_case
            "update terms and conditions"
            `Quick
            I18n_test.update_terms_and_conditions
        ] )
    ; ( "participation"
      , [ test_case "create participation" `Quick Participation_test.create
        ; test_case
            "mark participation as canceled"
            `Quick
            Participation_test.canceled
        ; test_case
            "set attendance on participation"
            `Quick
            Participation_test.set_attendance
        ] )
    ; ( "invitation"
      , [ test_case "create invitation" `Quick Invitation_test.create
        ; test_case "resend invitation" `Quick Invitation_test.resend
        ] )
    ; ( "experiment"
      , [ test_case "create experiment" `Quick Experiment_test.create
        ; test_case
            "create experiment without title"
            `Quick
            Experiment_test.create_without_title
        ; test_case "upate experiment" `Quick Experiment_test.update
        ; test_case
            "delete experiment with sessions"
            `Quick
            Experiment_test.delete_with_sessions
        ] )
    ]
;;
