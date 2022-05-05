Printexc.record_backtrace true

let () =
  let open Alcotest in
  run
    "cqrs commands"
    [ ( "subject"
      , [ test_case
            "sign up not allowed suffix"
            `Quick
            Subject_test.sign_up_not_allowed_suffix
        ; test_case "sign up" `Quick Subject_test.sign_up
        ; test_case
            "delete with unverified email"
            `Quick
            Subject_test.delete_unverified
        ; test_case
            "try delete with verified email"
            `Quick
            Subject_test.delete_verified
        ; test_case
            "update language of user"
            `Quick
            Subject_test.update_language
        ; test_case "update if user is paused" `Quick Subject_test.update_paused
        ; test_case "update user" `Quick Subject_test.update_full
        ; test_case "update password" `Quick Subject_test.update_password
        ; test_case
            "update password with wrong confirmation"
            `Quick
            Subject_test.update_password_wrong_confirmation
        ; test_case
            "update to short password according to policy"
            `Quick
            Subject_test.update_password_wrong_policy
        ; test_case
            "request validation for new email address"
            `Quick
            Subject_test.request_email_validation
        ; test_case
            "request validation for wrong email suffix"
            `Quick
            Subject_test.request_email_validation_wrong_suffix
        ; test_case "update email" `Quick Subject_test.update_email
        ; test_case "verify email" `Quick Subject_test.verify_email
        ; test_case
            "accept terms and condition"
            `Quick
            Subject_test.accept_terms_and_conditions
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
