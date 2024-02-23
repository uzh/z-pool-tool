Printexc.record_backtrace true

let () =
  let open Alcotest in
  run
    "cqrs commands"
    [ ( "contact"
      , [ test_case
            "sign up not allowed suffix"
            `Quick
            Contact_test.sign_up_not_allowed_suffix
        ; test_case "sign up" `Quick Contact_test.sign_up
        ; test_case
            "delete with unverified email"
            `Quick
            Contact_test.delete_unverified
        ; test_case
            "try delete with verified email"
            `Quick
            Contact_test.delete_verified
        ; test_case
            "update language of user"
            `Quick
            Contact_test.update_language
        ; test_case "update password" `Quick Contact_test.update_password
        ; test_case
            "password policy: min length"
            `Quick
            Contact_test.password_min_length
        ; test_case
            "password policy: capital letter"
            `Quick
            Contact_test.password_capital_letter
        ; test_case
            "password policy: number"
            `Quick
            Contact_test.password_number
        ; test_case
            "password policy: special character"
            `Quick
            Contact_test.password_special_char
        ; test_case
            "phone number: valid swiss number"
            `Quick
            Contact_test.valid_swiss_number
        ; test_case
            "phone number: valid german number"
            `Quick
            Contact_test.valid_german_number
        ; test_case
            "password policy: valid password"
            `Quick
            Contact_test.valid_password
        ; test_case
            "update password with wrong current password"
            `Quick
            Contact_test.update_password_wrong_current_password
        ; test_case
            "update to short password according to policy"
            `Quick
            Contact_test.update_password_wrong_policy
        ; test_case
            "update password with wrong confirmation"
            `Quick
            Contact_test.update_password_wrong_confirmation
        ; test_case
            "request validation for new email address"
            `Quick
            Contact_test.request_email_validation
        ; test_case
            "request validation for wrong email suffix"
            `Quick
            Contact_test.request_email_validation_wrong_suffix
        ; test_case "update email" `Quick Contact_test.update_email
        ; test_case "verify email" `Quick Contact_test.verify_email
        ; test_case
            "accept terms and condition"
            `Quick
            Contact_test.accept_terms_and_conditions
        ] )
    ; ( "tenant"
      , [ test_case
            "create tenant smtp auth"
            `Quick
            Tenant_test.create_smtp_auth
        ; test_case
            "create tenant smtp auth and force default"
            `Quick
            Tenant_test.create_smtp_force_defaut
        ; test_case
            "update tenant smtp auth"
            `Quick
            Tenant_test.update_smtp_auth
        ; test_case
            "delete tenant smtp auth"
            `Quick
            Tenant_test.delete_smtp_auth
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
      , [ test_case
            "update terms and conditions"
            `Quick
            I18n_test.update_terms_and_conditions
        ] )
    ; ( "assignment"
      , [ test_case "create assignment" `Quick Assignment_test.create
        ; test_case
            "create assignment with experiment smtp auth"
            `Quick
            Assignment_test.create_with_experiment_smtp
        ; test_case
            "mark assignment as canceled"
            `Quick
            Assignment_test.canceled
        ; test_case
            "mark assignment as canceled with closed session"
            `Quick
            Assignment_test.canceled_with_closed_session
        ; test_case
            "set attendance on assignment"
            `Quick
            Assignment_test.set_attendance
        ; test_case
            "set invalid attendance on assignment"
            `Quick
            Assignment_test.set_invalid_attendance
        ; test_case
            "assignment validation"
            `Quick
            Assignment_test.assignment_validation
        ; test_case
            "set attendance missing data id"
            `Quick
            Assignment_test.set_attendance_missing_data_id
        ; test_case
            "set attendance with data id"
            `Quick
            Assignment_test.set_attendance_with_data_id
        ; test_case
            "assign to fully booked session"
            `Quick
            Assignment_test.assign_to_fully_booked_session
        ; test_case
            "assign to canceled session"
            `Quick
            Assignment_test.assign_to_canceled_session
        ; test_case
            "assign to session contact is already assigned"
            `Quick
            Assignment_test.assign_to_session_contact_is_already_assigned
        ; test_case
            "assign to experiment with direct registration disabled"
            `Quick
            Assignment_test
            .assign_to_experiment_with_direct_registration_disabled
        ; test_case
            "assign to experiment with direct registration disabled as admin"
            `Quick
            Assignment_test
            .assign_to_experiment_with_direct_registration_disabled_as_admin
        ; test_case
            "assign user from waiting list"
            `Quick
            Assignment_test.assign_contact_from_waiting_list
        ; test_case
            "assign contact from waiting list with follow-ups"
            `Quick
            Assignment_test.assign_contact_from_waiting_list_with_follow_ups
        ; test_case
            "assign contact from waiting list to disabled experiment"
            `Quick
            Assignment_test
            .assign_contact_from_waiting_list_to_disabled_experiment
        ; test_case
            "assign to session with follow ups"
            `Quick
            Assignment_test.assign_to_session_with_follow_ups
        ; test_case
            "mark uncanceled as deleted"
            `Quick
            Assignment_test.mark_uncanceled_as_deleted
        ; test_case
            "mark canceled as deleted"
            `Quick
            Assignment_test.marked_canceled_as_deleted
        ; test_case
            "marked closed assignment with followup as deleted"
            `Quick
            Assignment_test.marked_closed_with_followups_as_deleted
        ; test_case
            "cancel deleted assignment"
            `Quick
            Assignment_test.cancel_deleted_assignment
        ; test_case
            "send reminder invalid"
            `Quick
            Assignment_test.send_reminder_invalid
        ; test_case
            "swap session without notification"
            `Quick
            Assignment_test.swap_session_without_notification
        ; test_case
            "swap session with notification"
            `Quick
            Assignment_test.swap_session_with_notification
        ; test_case
            "swap session to past session"
            `Quick
            Assignment_test.swap_session_to_past_session
        ] )
    ; ( "invitation"
      , [ test_case "create invitation" `Quick Invitation_test.create
        ; test_case
            "create invitation with experiment smtp"
            `Quick
            Invitation_test.create_with_experiment_smtp
        ; test_case "resend invitation" `Quick Invitation_test.resend
        ] )
    ; ( "experiment"
      , [ test_case "create experiment" `Quick Experiment_test.create
        ; test_case
            "create experiment without title"
            `Quick
            Experiment_test.create_without_title
        ; test_case "update experiment" `Quick Experiment_test.update
        ; test_case
            "add ou and contact person"
            `Quick
            Experiment_test.update_add_ou_and_contact_person
        ; test_case "remove ou" `Quick Experiment_test.update_remove_ou
        ; test_case
            "delete experiment with sessions"
            `Quick
            Experiment_test.delete_with_sessions
        ; test_case
            "delete experiment with filter"
            `Quick
            Experiment_test.delete_with_filter
        ] )
    ; ( "waiting list"
      , [ test_case "sign up" `Quick Waiting_list_test.create
        ; test_case "sign off" `Quick Waiting_list_test.delete
        ; test_case
            "create with direct registration enabled"
            `Quick
            Waiting_list_test.create_with_direct_registration_enabled
        ; test_case "update comment" `Quick Waiting_list_test.update
        ] )
    ; ( "user"
      , [ test_case
            "validate email addresses"
            `Quick
            User_test.validate_email_adress
        ] )
    ; "location", [ test_case "create location" `Quick Location_test.create ]
    ; ( "mailing"
      , [ test_case "create mailing" `Quick Mailing_test.create
        ; test_case
            "create mailing with distribution"
            `Quick
            Mailing_test.create_with_distribution
        ; test_case
            "create mailing with end before start"
            `Quick
            Mailing_test.create_end_before_start
        ; test_case
            "create mailing with start now"
            `Quick
            Mailing_test.create_with_start_now
        ] )
    ; ( "session"
      , Session_test.
          [ test_case "create session empty data fails" `Quick create_empty_data
          ; test_case
              "create session invalid data fails"
              `Quick
              create_invalid_data
          ; test_case
              "create session min participants greater than max participants \
               fails"
              `Quick
              create_min_gt_max
          ; test_case
              "create session optionals omitted succeeds"
              `Quick
              create_no_optional
          ; test_case "create session all info succeeds" `Quick create_full
          ; test_case
              "create session min participants equal max participants succeeds"
              `Quick
              create_min_eq_max
          ; test_case "update session empty data fails" `Quick update_empty_data
          ; test_case
              "update session invalid data fails"
              `Quick
              update_invalid_data
          ; test_case
              "update session min participants greater than max participants \
               fails"
              `Quick
              update_min_gt_max
          ; test_case
              "update session optionals omitted succeeds"
              `Quick
              update_no_optional
          ; test_case "update session all info succeeds" `Quick update_full
          ; test_case
              "update session min participants greater than max participants \
               succeeds"
              `Quick
              update_min_eq_max
          ; test_case "delete session succeeds" `Quick delete
          ; test_case "delete closed session fails" `Quick delete_closed_session
          ; test_case
              "delete session with assignments fails"
              `Quick
              delete_session_with_assignments
          ; test_case
              "delete session with follow ups fails"
              `Quick
              delete_session_with_follow_ups
          ; test_case
              "cancel session without reason fails"
              `Quick
              cancel_no_reason
          ; test_case
              "cancel session without message channels fails"
              `Quick
              cancel_no_message_channels
          ; test_case "cancel session in past fails" `Quick cancel_in_past
          ; test_case
              "cancel session already canceled fails"
              `Quick
              cancel_already_canceled
          ; test_case "cancel session succeeds" `Quick cancel_valid
          ; test_case
              "cancel valid with missing phone number"
              `Quick
              cancel_valid_with_missing_cell_phone
          ; test_case
              "cancel with email and text notification"
              `Quick
              cancel_with_email_and_text_notification
          ; test_case "close session" `Quick close_valid
          ; test_case
              "close session with valid assignments"
              `Quick
              close_valid_with_assignments
          ; test_case
              "close session with deleted assignment"
              `Quick
              close_with_deleted_assignment
          ; test_case
              "close session with invalid participation"
              `Quick
              validate_invalid_participation
          ; test_case
              "close unparticipated assignment with followup"
              `Quick
              close_unparticipated_with_followup
          ; test_case
              "create follow up earlier than parent fails"
              `Quick
              create_follow_up_earlier
          ; test_case
              "create follow up later than parent succeeds"
              `Quick
              create_follow_up_later
          ; test_case
              "update follow up earlier than parent fails"
              `Quick
              update_follow_up_earlier
          ; test_case
              "update follow up later than parent succeeds"
              `Quick
              update_follow_up_later
          ; test_case
              "update with follow ups earlier fails"
              `Quick
              update_follow_ups_earlier
          ; test_case
              "update with follow ups later succeeds"
              `Quick
              update_follow_ups_later
          ; test_case "reschedule session to past" `Quick reschedule_to_past
          ; test_case
              "reschedule with experiment smtp auth"
              `Quick
              reschedule_with_experiment_smtp
          ; test_case "resend reminders invalid" `Quick resend_reminders_invalid
          ; test_case "resend reminders valid" `Quick resend_reminders_valid
          ; test_case
              "duplicate single session"
              `Quick
              Duplication.single_session
          ; test_case
              "duplicate session with followup"
              `Quick
              Duplication.with_followup
          ; test_case
              "duplicate with missing value "
              `Quick
              Duplication.missing_value
          ; test_case
              "duplicate with followup starting before main "
              `Quick
              Duplication.followup_before_main
          ] )
    ; ( "custom_field"
      , [ test_case "create custom field" `Quick Custom_field_test.create
        ; test_case
            "create custom field with missing name"
            `Quick
            Custom_field_test.create_with_missing_name
        ; test_case "update custom field" `Quick Custom_field_test.update
        ; test_case
            "update field type of published field"
            `Quick
            Custom_field_test.update_type_of_published_field
        ; test_case
            "crate custom field option"
            `Quick
            Custom_field_test.create_option
        ; test_case
            "delete published field"
            `Quick
            Custom_field_test.delete_published_field
        ; test_case
            "delete published field option"
            `Quick
            Custom_field_test.delete_published_option
        ; test_case
            "publish field without options"
            `Quick
            Custom_field_test.publish_field_without_options
        ; test_case
            "publish field with options"
            `Quick
            Custom_field_test.publish_field_with_options
        ; test_case
            "validate string custom field"
            `Quick
            Custom_field_test.ValidationTests.validate_text_field
        ; test_case
            "validate number custom field"
            `Quick
            Custom_field_test.ValidationTests.validate_number_field
        ; test_case
            "validate multi select custom field"
            `Quick
            Custom_field_test.ValidationTests.validate_multi_select_field
        ; test_case
            "update visibility settings"
            `Quick
            Custom_field_test.Settings.update_visibility
        ] )
    ; ( "matcher"
      , [ test_case
            "create invitations"
            `Quick
            Matcher_test.create_invitations_model
        ] )
    ; ( "message template"
      , [ test_case
            "create message template"
            `Quick
            Message_template_test.create
        ; test_case
            "create message template with invalid language"
            `Quick
            Message_template_test.create_with_unavailable_language
        ; test_case
            "delete  with entity uuid"
            `Quick
            Message_template_test.delete_valid
        ; test_case
            "delete without entity uuid"
            `Quick
            Message_template_test.delete_without_entity
        ] )
    ; ( "queue"
      , Message_history.Resend.
          [ test_case "resend pending job" `Quick resend_pending
          ; test_case "resend unchanged" `Quick resend_unchanged
          ; test_case "resend updated recipient" `Quick resend_updated_recipient
          ; test_case "resend updated smtp" `Quick resend_updated_smtp
          ] )
    ; ( "organisational unit"
      , [ test_case
            "create orgnisatinal unit succeeds"
            `Quick
            Organisational_unit_test.create_succeeds
        ; test_case
            "create orgnisatinal unit fails"
            `Quick
            Organisational_unit_test.create_fails
        ] )
    ; ( "user import"
      , [ test_case
            "confirm without matching password"
            `Quick
            User_import_test.confirm_without_matching_password
        ; test_case "confirm as admin" `Quick User_import_test.confirm_as_admin
        ; test_case
            "confirm as contact"
            `Quick
            User_import_test.confirm_as_contact
        ] )
    ; ( "tags"
      , [ test_case "create tag succeeds" `Quick Tag_test.create_event
        ; test_case "update tag succeeds" `Quick Tag_test.update_event
        ; test_case
            "assign auto tag to experiment"
            `Quick
            Tag_test.assign_auto_tag_to_experiment
        ; test_case
            "remove auto tag from experiment"
            `Quick
            Tag_test.remove_auto_tag_from_experiment
        ] )
    ]
;;
