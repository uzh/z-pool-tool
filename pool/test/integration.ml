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
    ; ( "partial_update"
      , Partial_update.
          [ test_case "update with old version" `Quick update_with_old_version
          ; test_case "update custom field answer" `Quick update_custom_field
          ; test_case
              "update custom field with invalid answer"
              `Quick
              update_custom_field_with_invalid_answer
          ; test_case
              "update admin input only field as user"
              `Quick
              update_admin_input_only_field_as_user
          ; test_case
              "update non overwrite field as admin"
              `Quick
              update_non_overwrite_field_as_admin
          ] )
    ; ( "filter"
      , Filter_test.
          [ test_case "filter contacts" `Quick filter_contacts
          ; test_case "filter by email and custom field" `Quick filter_by_email
          ; test_case
              "validate filter with unknown field"
              `Quick
              validate_filter_with_unknown_field
          ; test_case
              "validate filter with invalid value"
              `Quick
              validate_filter_with_invalid_value
          ; test_case "filter contains all" `Quick filter_by_list_contains_all
          ; test_case "filter contains none" `Quick filter_by_list_contains_none
          ; test_case "filter contains some" `Quick filter_by_list_contains_some
          ] )
    ; ( "matcher"
      , Matcher_test.
          [ test_case "send invitations" `Quick create_invitations_repo ] )
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
     let ctx = Pool_tenant.to_ctx Test_utils.Data.database_label in
     let%lwt _ = Sihl.Container.start_services services in
     (* [save_rules] will fail on permissions that already exist, so we need to
        clear the root database first in order to make sure new
        [root_permissions] items end up in the database *)
     let%lwt () =
       (* TODO: Handle errors *)
       Lwt_list.fold_left_s
         (fun _acc perm ->
           let%lwt _rv = Guard.Persistence.delete_rule ~ctx perm in
           Lwt.return ())
         ()
         Guard.root_permissions
     in
     let%lwt (_
               : ( Guard.Persistence.auth_rule list
                 , Guard.Persistence.auth_rule list )
                 result)
       =
       Guard.Persistence.save_rules ~ctx Guard.root_permissions
     in
     Alcotest_lwt.run "integration" @@ suite)
;;
