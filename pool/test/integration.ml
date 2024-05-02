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
          ; test_case "login after terms update" `Slow login_after_terms_update
          ; test_case "can create smtp auth" `Slow create_smtp_auth
          ; test_case "can delete smtp auth" `Slow delete_smtp_auth
          ] )
    ; ( "dev/test"
      , [ test_case "intercept email" `Slow Common_test.validate_email ] )
    ; ( "authorization"
      , Authorization_test.
          [ test_case
              "permit valid operation"
              `Slow
              recruiter_can_update_contact_language
          ; test_case
              "deny invalid operation"
              `Slow
              guest_cannot_update_language
          ; test_case "use parametric roles" `Slow operator_works
          ; test_case
              "test adding of role assignments"
              `Slow
              Admin_role_assignment.assignable_roles
          ; test_case
              "grant valid and invalid roles"
              `Slow
              Admin_role_assignment.grant_roles
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
              "set value of required field to null"
              `Slow
              set_value_of_required_field_to_null
          ] )
    ; ( "filter"
      , Filter_test.
          [ test_case "update filter" `Slow update_filter
          ; test_case
              "create and update filter template"
              `Slow
              create_and_update_filter_template
          ; test_case "filter contacts" `Slow filter_contacts
          ; test_case "filter by email and custom field" `Slow filter_by_email
          ; test_case "filter exclude inactive" `Slow filter_exclude_inactive
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
          ; test_case "filter by select field" `Slow filter_by_select_field
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
          ; test_case
              "filter by empty language"
              `Slow
              filter_by_empty_hardcoded_value
          ; test_case
              "filter by non-empty language"
              `Slow
              filter_by_non_empty_hardcoded_value
          ; test_case
              "filter by empty custom field"
              `Slow
              filter_by_empty_custom_field
          ; test_case
              "filter by nonempty custom field"
              `Slow
              filter_by_non_empty_custom_field
          ; test_case
              "filter by empty custom field with deleted answer"
              `Slow
              filter_by_empty_custom_field_with_deleted_value
          ; test_case
              "filter by date type custom field"
              `Slow
              filter_by_date_custom_field
          ] )
    ; ( "matcher"
      , Matcher_test.
          [ test_case "send invitations" `Slow create_invitations
          ; test_case "reset experiment invitations" `Slow reset_invitations
          ; test_case "matcher notifiaction" `Slow matcher_notification
          ] )
    ; ( "assignment job"
      , Assignment_job_test.
          [ test_case "initialize tests" `Slow init
          ; test_case "update without filter" `Slow update_without_filter
          ; test_case "exclude contact" `Slow exclude_contact
          ] )
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
              "get template without experiment language and templates"
              `Slow
              get_template_without_experiment_language_and_templates
          ; test_case
              "get template without experiment languages"
              `Slow
              get_template_without_experiment_language
          ; test_case
              "get template with experiment language"
              `Slow
              get_template_with_experiment_language
          ; test_case
              "get template with experiment language and template"
              `Slow
              get_template_with_experiment_language_and_template
          ; test_case
              "get templates in multiple languages"
              `Slow
              get_templates_in_multile_languages
          ; test_case
              "experiment invitation with sender"
              `Slow
              experiment_invitation_with_sender
          ; test_case
              "assignment creation with sender"
              `Slow
              assignment_creation_with_sender
          ] )
    ; Message_history.(
        ( "message_history"
        , [ test_case "initialize message history data" `Slow initialize
          ; test_case
              "account suspension message history"
              `Slow
              account_suspension_notification
          ; test_case
              "assignment confirmation message history"
              `Slow
              assignment_confirmation
          ; test_case
              "assignment session change message history"
              `Slow
              assignment_session_change
          ; test_case
              "contact email change attempt message history"
              `Slow
              contact_email_change_attempt
          ; test_case
              "contact registration attempt message history"
              `Slow
              contact_registration_attempt
          ; test_case
              "email verification attempt message history"
              `Slow
              email_verification
          ; test_case
              "experiment invitation message history"
              `Slow
              experiment_invitation
          ; test_case "password change message history" `Slow password_change
          ; test_case "password reset message history" `Slow password_reset
          ; test_case
              "phone verification message history"
              `Slow
              phone_verification
          ; test_case "session reminder message history" `Slow session_reminder
          ] ))
    ; ( "assignment"
      , Assignment_test.
          [ test_case
              "cancel assignment with follow ups"
              `Slow
              cancel_assignment_with_follow_ups
          ] )
    ; ( "session"
      , Session_test.
          [ test_case
              "close session and check contact figures"
              `Slow
              close_session_check_contact_figures
          ; test_case
              "send session reminders with default lead times"
              `Slow
              send_session_reminders_with_default_leat_time
          ] )
    ; ( "waiting lists"
      , Waiting_list_test.
          [ test_case
              "find pending waiting lists entries by contcat"
              `Slow
              PendingWaitingLists.find_pending_waitinglists_by_contact
          ; test_case
              "exclude experiment after signing up for a session"
              `Slow
              PendingWaitingLists.exclude_after_assignign_to_session
          ; test_case
              "include experiment after session cancellation"
              `Slow
              PendingWaitingLists.include_after_session_cancellation
          ] )
    ; ( "experiment"
      , Experiment_test.
          [ test_case "autofill public title" `Slow autofill_public_title
          ; test_case
              "list available experiments"
              `Slow
              AvailableExperiments.list_available_experiments
          ; test_case
              "exclude experiment after registration for session"
              `Slow
              AvailableExperiments
              .exclude_experiment_after_registration_for_session
          ; test_case
              "cancel session contact is assigned to"
              `Slow
              AvailableExperiments.cancel_session
          ; test_case
              "mark assignments as deleted"
              `Slow
              AvailableExperiments.mark_assignment_as_deleted
          ] )
    ; ( "filtering"
      , [ test_case
            "uninvited contact is listed"
            `Slow
            Filter_invitation_tests.finds_uninvited_contacts
        ; test_case
            "invited contact is not listed"
            `Slow
            Filter_invitation_tests.filters_out_invited_contacts
        ; test_case
            "unassigned contact is listed"
            `Slow
            Filter_assignment_tests.finds_unassigned_contacts
        ; test_case
            "assigned contact is not listed"
            `Slow
            Filter_assignment_tests.filters_out_assigned_contacts
        ] )
    ; ( "contact counter"
      , Contact_counter_test.
          [ test_case
              "invite contact: ssend invitation"
              `Slow
              InviteContact.invite
          ; test_case
              "attend all: register for session"
              `Slow
              AttendAll.register_for_session
          ; test_case
              "attend all: close first session"
              `Slow
              AttendAll.close_first_session
          ; test_case
              "attend all: close follow up session"
              `Slow
              AttendAll.close_follow_up_session
          ; test_case
              "delete attended: delete follow up"
              `Slow
              DeleteAttended.delete_follow_up
          ; test_case
              "delete attended: delete main"
              `Slow
              DeleteAttended.delete_main
          ; test_case
              "cancel session: without follow up"
              `Slow
              CancelSession.without_followups
          ; test_case "cancel session: follow up" `Slow CancelSession.follow_up
          ; test_case
              "cancel session: main with follow up"
              `Slow
              CancelSession.main_with_follow_up
          ; test_case
              "Do not attend: register for session"
              `Slow
              DoNotAttend.register_for_session
          ; test_case
              "Do not attend: close main session"
              `Slow
              DoNotAttend.close_main
          ; test_case
              "no show: register for session"
              `Slow
              NoShow.register_for_session
          ; test_case "no show: close main" `Slow NoShow.close_main
          ; test_case
              "delete unattended: register"
              `Slow
              DeleteUnattended.register_for_session
          ; test_case
              "delete unattended: delete main session assignment"
              `Slow
              DeleteUnattended.delete_main
          ; test_case
              "update closed: cannot delete of unclosed session"
              `Slow
              UpdateAssignments.update_unclosed
          ; test_case
              "update closed: close main session"
              `Slow
              UpdateAssignments.close_main_session
          ; test_case
              "update closed: update assignment manually"
              `Slow
              UpdateAssignments.update_assignment_manually
          ; test_case
              "update online assignment manually"
              `Slow
              UpdateAssignments.update_online_assignment
          ; test_case
              "update closed: close follow up session"
              `Slow
              UpdateAssignments.close_followup_session
          ; test_case
              "update closed: update follow up assignment manually"
              `Slow
              UpdateAssignments.update_follow_up_assignment_manually
          ] )
    ; ( "tagging"
      , Tag_test.
          [ test_case "create tag" `Slow create_persistent
          ; test_case "create duplicate tag" `Slow create_persistent_fail
          ; test_case "update tag" `Slow update_persistent
          ; test_case "assign tag to contact" `Slow assign_tag_to_contact
          ; test_case "remove tag from contact" `Slow remove_tag_from_contact
          ; test_case
              "try to assign experiment tag to contact"
              `Slow
              try_assign_experiment_tag_to_contact
          ; test_case "filter tags" `Slow Filter_test.filter_by_tags
          ] )
    ; ( "user import"
      , User_import_test.
          [ test_case "confirm as contact" `Slow confirm_as_contact_integration
          ; test_case
              "find contacts to notify"
              `Slow
              Repo.find_contacts_to_notify
          ; test_case
              "find contacts to remind"
              `Slow
              Repo.find_contacts_to_remind
          ] )
    ; ( "time window"
      , Time_window_test.
          [ test_case "confirm as contact" `Slow find_overlapping ] )
    ; "cleanup", [ test_case "clean up test database" `Slow Test_seed.cleanup ]
    ]
;;

let services =
  [ Database.register ()
  ; Service.User.register ()
  ; Service.Token.register ()
  ; Email.Service.register ()
  ; Email.Service.Queue.register ()
  ; Service.Storage.register ()
  ]
;;

let () =
  Lwt_main.run
    (let open Test_utils in
     let%lwt () = setup_test () in
     let%lwt _ = Sihl.Container.start_services services in
     let%lwt () = Test_seed.create Data.database_label () in
     Alcotest_lwt.run "integration" @@ suite)
;;
