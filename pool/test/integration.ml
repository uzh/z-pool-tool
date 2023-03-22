Printexc.record_backtrace true

let suite =
  Alcotest_lwt.
    [ ( "database"
      , Database_test.
          [ test_case "access root" `Slow check_root_database
          ; test_case "find tenants" `Slow check_find_tenant_database
          ; test_case "access tenant" `Slow check_tenant_database
          ] )
    ; ( "settings"
      , Tenant_settings_test.
          [ test_case "read contact email" `Slow check_contact_email
          ; test_case "has email suffixes" `Slow check_email_suffix
          ; test_case
              "read inactive user disable after"
              `Slow
              check_inactive_user_disable_after
          ; test_case
              "read inactive user warning after"
              `Slow
              check_inactive_user_warning
          ; test_case "read languages" `Slow check_languages
          ; test_case
              "has terms and conditions"
              `Slow
              check_terms_and_conditions
          ; test_case
              "update terms and conditions"
              `Slow
              update_terms_and_conditions
          ; test_case "login after terms update" `Slow login_after_terms_update
          ] )
    ; ( "dev/test"
      , [ test_case "intercept email" `Slow Common_test.validate_email ] )
    ; ( "authorization"
      , Authorization_test.
          [ test_case "permit valid operation" `Slow admin_can_update_language
          ; test_case
              "deny invalid operation"
              `Slow
              guest_cannot_update_language
          ; test_case "use parametric roles" `Slow operator_works
          ] )
    ; ( "partial_update"
      , Partial_update.
          [ test_case "update with old version" `Slow update_with_old_version
          ; test_case "update custom field answer" `Slow update_custom_field
          ; test_case
              "update custom field with invalid answer"
              `Slow
              update_custom_field_with_invalid_answer
          ; test_case
              "update admin input only field as user"
              `Slow
              update_admin_input_only_field_as_user
          ; test_case
              "update non override field as admin"
              `Slow
              update_non_override_field_as_admin
          ; test_case
              "set value of none required field to null"
              `Slow
              set_value_of_none_required_field_to_null
          ; test_case
              "set value of  required field to null"
              `Slow
              set_value_of_required_field_to_null
          ] )
    ; ( "filter"
      , Filter_test.
          [ test_case "filter contacts" `Slow filter_contacts
          ; test_case "filter by email and custom field" `Slow filter_by_email
          ; test_case
              "validate filter with unknown field"
              `Slow
              validate_filter_with_unknown_field
          ; test_case
              "validate filter with invalid value"
              `Slow
              validate_filter_with_invalid_value
          ; test_case "filter contains all" `Slow filter_by_list_contains_all
          ; test_case "filter contains none" `Slow filter_by_list_contains_none
          ; test_case "filter contains some" `Slow filter_by_list_contains_some
          ; test_case
              "retrieve ordered contacts"
              `Slow
              retrieve_fitleterd_and_ordered_contacts
          ; test_case
              "create filter template with template"
              `Slow
              create_filter_template_with_template
          ; test_case
              "test filter with admin overridden values"
              `Slow
              filter_with_admin_value
          ; test_case
              "no admin values shown to contacts"
              `Slow
              no_admin_values_shown_to_contacts
          ; test_case
              "filter ignore admin value"
              `Slow
              filter_ignore_admin_value
          ; test_case
              "filter by experiment participation"
              `Slow
              filter_by_experiment_participation
          ] )
    ; ( "matcher"
      , Matcher_test.
          [ test_case "send invitations" `Slow create_invitations_repo ] )
    ; ( "contact"
      , Contact_test.
          [ test_case
              "should not send registration notification"
              `Slow
              should_not_send_registration_notification
          ] )
    ; ( "message_template"
      , Message_template_test.
          [ test_case
              "get template with language missing"
              `Slow
              get_template_with_language_missing
          ; test_case
              "get templates in multiple languages"
              `Slow
              get_templates_in_multile_languages
          ] )
    ; ( "assignment"
      , Assignment_test.
          [ test_case
              "cancel assignment with follow ups"
              `Slow
              cancel_assignment_with_follow_ups
          ] )
    ; ( "waiting lists"
      , Waiting_list_test.
          [ test_case
              "find pending waiting lists entries by contcat"
              `Slow
              PendingWaitingLists.find_pending_waitinglists_by_contact
          ] )
    ; "cleanup", [ test_case "clean up test database" `Quick Seed.cleanup ]
    ]
;;

let services =
  [ Database.register ()
  ; Service.User.register ()
  ; Service.Token.register ()
  ; Pool_tenant.Service.Email.register ()
  ; Pool_tenant.Service.Queue.register ()
  ; Service.Storage.register ()
  ]
;;

let () =
  Lwt_main.run
    (let open Test_utils in
     let%lwt () = setup_test () in
     let%lwt _ = Sihl.Container.start_services services in
     let%lwt () = Seed.create Data.database_label () in
     Alcotest_lwt.run "integration" @@ suite)
;;
