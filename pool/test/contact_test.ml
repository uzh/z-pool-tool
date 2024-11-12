open CCFun.Infix
open Pool_message
module Contact_command = Cqrs_command.Contact_command
module Language = Pool_common.Language

let current_user = Test_utils.Model.create_admin ()

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let contact_info email_address =
  email_address, "Password1!", "Jane", "Doe", Some Language.En
;;

let allowed_email_suffixes =
  [ "gmail.com" ]
  |> CCList.map Settings.EmailSuffix.create
  |> CCResult.flatten_l
  |> CCResult.get_exn
;;

let tenant = Tenant_test.Data.full_tenant |> CCResult.get_exn

let confirmation_mail contact =
  let email =
    Contact.(contact |> email_address |> Pool_user.EmailAddress.value)
  in
  let open Message_template in
  let sender = "test@econ.uzh.ch" in
  let ({ email_subject; email_text; label; _ } : Message_template.t) =
    Test_utils.Model.create_message_template ()
  in
  Sihl_email.
    { sender
    ; recipient = email
    ; subject = email_subject |> EmailSubject.value
    ; text = ""
    ; html = Some (email_text |> EmailText.value)
    ; cc = []
    ; bcc = []
    }
  |> Email.Service.Job.create
  |> Email.create_dispatch
       ~job_ctx:
         (Pool_queue.job_ctx_create Contact.[ contact |> id |> Id.to_common ])
       ~message_template:(Message_template.Label.show label)
;;

let sign_up_contact contact_info =
  let email_address, password, firstname, lastname, _ = contact_info in
  [ Field.(Email |> show), [ email_address ]
  ; Field.(Password |> show), [ password ]
  ; Field.(Firstname |> show), [ firstname ]
  ; Field.(Lastname |> show), [ lastname ]
  ]
;;

let create_contact verified contact_info =
  let open Pool_user in
  let email_address, _, firstname, lastname, language = contact_info in
  { Contact.user =
      { id = Id.(create ())
      ; email = EmailAddress.of_string email_address
      ; lastname = Lastname.of_string lastname
      ; firstname = Firstname.of_string firstname
      ; status = Status.Active
      ; admin = Pool_user.IsAdmin.create false
      ; confirmed = Pool_user.Confirmed.create true
      }
  ; terms_accepted_at = Pool_user.TermsAccepted.create_now () |> CCOption.pure
  ; language
  ; experiment_type_preference = None
  ; cell_phone = None
  ; paused = Pool_user.Paused.create false
  ; disabled = Pool_user.Disabled.create false
  ; verified = None
  ; email_verified =
      (if verified
       then Some (Ptime_clock.now () |> Pool_user.EmailVerified.create)
       else None)
  ; num_invitations = Contact.NumberOfInvitations.init
  ; num_assignments = Contact.NumberOfAssignments.init
  ; num_show_ups = Contact.NumberOfShowUps.init
  ; num_no_shows = Contact.NumberOfNoShows.init
  ; num_participations = Contact.NumberOfParticipations.init
  ; firstname_version = Pool_common.Version.create ()
  ; lastname_version = Pool_common.Version.create ()
  ; paused_version = Pool_common.Version.create ()
  ; language_version = Pool_common.Version.create ()
  ; experiment_type_preference_version = Pool_common.Version.create ()
  ; import_pending = Pool_user.ImportPending.create false
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

let verification_email (email_address, _, _, _, _) =
  let open Message_template in
  let sender = "test@econ.uzh.ch" in
  let ({ email_subject; email_text; label; _ } : Message_template.t) =
    Test_utils.Model.create_message_template ()
  in
  Sihl_email.
    { sender
    ; recipient = email_address
    ; subject = email_subject |> EmailSubject.value
    ; text = ""
    ; html = Some (email_text |> EmailText.value)
    ; cc = []
    ; bcc = []
    }
  |> Email.Service.Job.create
  |> Email.create_dispatch ~message_template:(Message_template.Label.show label)
;;

let sign_up_not_allowed_suffix () =
  let open Contact_command.SignUp in
  let allowed_email_suffixes =
    [ "gmail.com" ]
    |> CCList.map Settings.EmailSuffix.create
    |> CCResult.flatten_l
    |> CCResult.get_exn
  in
  let contact_info = "john@bluewin.com" |> contact_info in
  let ({ Cqrs_command.User_command.email; _ } as decoded) =
    contact_info |> sign_up_contact |> decode |> CCResult.get_exn
  in
  let token = Email.Token.create "testtoken" in
  let verification_email = verification_email contact_info in
  let events =
    decoded
    |> handle ~allowed_email_suffixes [] token email verification_email None
  in
  let expected =
    Error
      (Error.InvalidEmailSuffix
         (allowed_email_suffixes |> CCList.map Settings.EmailSuffix.value))
  in
  check_result expected events
;;

let sign_up () =
  let user_id = Contact.Id.create () in
  let terms_accepted_at =
    Pool_user.TermsAccepted.create_now () |> CCOption.pure
  in
  let ((email_address, password, firstname, lastname, language) as contact_info)
    =
    contact_info "john@gmail.com"
  in
  let email =
    "john@gmail.com" |> Pool_user.EmailAddress.create |> CCResult.get_exn
  in
  let token = Email.Token.create "testtoken" in
  let verification_email = verification_email contact_info in
  let events =
    let open CCResult in
    let open Contact_command.SignUp in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    contact_info
    |> sign_up_contact
    |> decode
    |> Pool_common.Utils.get_or_failwith
    |> handle
         ~allowed_email_suffixes
         ~user_id
         ~terms_accepted_at
         []
         token
         email
         verification_email
         language
  in
  let expected =
    let email = email_address |> Pool_user.EmailAddress.of_string in
    let firstname = firstname |> Pool_user.Firstname.of_string in
    let lastname = lastname |> Pool_user.Lastname.of_string in
    let contact : Contact.create =
      { Contact.user_id
      ; email
      ; password = password |> Pool_user.Password.Plain.create
      ; firstname
      ; lastname
      ; terms_accepted_at
      ; language
      }
    in
    Ok
      [ Contact.Created contact |> Pool_event.contact
      ; Email.Created (email, token, user_id |> Contact.Id.to_user)
        |> Pool_event.email_verification
      ; Email.sent verification_email |> Pool_event.email
      ]
  in
  check_result expected events
;;

let delete_unverified () =
  let contact = "john@gmail.com" |> contact_info |> create_contact false in
  let events = Contact_command.DeleteUnverified.handle contact in
  let expected =
    Ok [ Contact.UnverifiedDeleted contact |> Pool_event.contact ]
  in
  check_result expected events
;;

let delete_verified () =
  let contact = "john@gmail.com" |> contact_info |> create_contact true in
  let events = Contact_command.DeleteUnverified.handle contact in
  let expected = Error Error.EmailDeleteAlreadyVerified in
  check_result expected events
;;

let update_language () =
  let open CCResult in
  let contact = "john@gmail.com" |> contact_info |> create_contact true in
  let language = Language.De in
  let version = 0 |> Pool_common.Version.of_int in
  let partial_update =
    Custom_field.PartialUpdate.(Language (version, Some language))
  in
  let events =
    partial_update
    |> Contact_command.Update.handle (Pool_context.Contact contact) contact
  in
  let expected =
    Ok
      [ Custom_field.PartialUpdate
          (partial_update, contact, Pool_context.Contact contact)
        |> Pool_event.custom_field
      ]
  in
  check_result expected events
;;

let update_password () =
  let ((_, password, _, _, _) as contact_info) =
    "john@gmail.com" |> contact_info
  in
  let contact = contact_info |> create_contact true in
  let new_password = "NewPassword2!" in
  let notification = confirmation_mail contact in
  let events =
    let open Cqrs_command.User_command.UpdatePassword in
    [ Field.(CurrentPassword |> show), [ password ]
    ; Field.(NewPassword |> show), [ new_password ]
    ; Field.(PasswordConfirmation |> show), [ new_password ]
    ]
    |> decode
    |> Pool_common.Utils.get_or_failwith
    |> handle ~notification Contact.(contact |> id |> Id.to_user)
  in
  let expected =
    Ok
      [ Pool_user.PasswordUpdated
          ( Contact.(contact |> id |> Id.to_user)
          , password |> Pool_user.Password.Plain.create
          , new_password |> Pool_user.Password.Plain.create
          , new_password |> Pool_user.Password.Confirmation.create )
        |> Pool_event.user
      ; Email.sent notification |> Pool_event.email
      ]
  in
  check_result expected events
;;

let validate_password_policy password expected =
  let res = Pool_user.Password.Plain.(password |> create |> validate) in
  Alcotest.(
    check Test_utils.(result password_plain error) "succeeds" expected res)
;;

let password_min_length () =
  validate_password_policy "Pass9!" (Error (Error.PasswordPolicyMinLength 8))
;;

let password_capital_letter () =
  validate_password_policy
    "password9!"
    (Error Error.PasswordPolicyCapitalLetter)
;;

let password_number () =
  validate_password_policy "Password?" (Error Error.PasswordPolicyNumber)
;;

let password_special_char () =
  validate_password_policy
    "Password9"
    (Error
       (Error.PasswordPolicySpecialChar
          Pool_user.Password.Policy.default_special_char_set))
;;

let valid_password () =
  let password = "Password9*" in
  let expected =
    password |> Pool_user.Password.Plain.create |> CCResult.return
  in
  validate_password_policy password expected
;;

let validate_cell_phone nr expected =
  let res = nr |> Pool_user.CellPhone.create in
  Alcotest.(check Test_utils.(result phone_nr error) "succeeds" expected res)
;;

let valid_swiss_number () =
  let nr = "+41791234567" in
  let expected =
    nr
    |> Pool_user.CellPhone.create
    |> Test_utils.get_or_failwith
    |> CCResult.return
  in
  validate_cell_phone nr expected
;;

let valid_german_number () =
  let nr = "+491512345678" in
  let expected =
    nr
    |> Pool_user.CellPhone.create
    |> Test_utils.get_or_failwith
    |> CCResult.return
  in
  validate_cell_phone nr expected
;;

let update_password_wrong_policy () =
  let open CCResult.Infix in
  let ((_, password, _, _, _) as contact_info) =
    "john@gmail.com" |> contact_info
  in
  let contact = contact_info |> create_contact true in
  let new_password = "Short1!" in
  let notification = confirmation_mail contact in
  let events =
    let open Cqrs_command.User_command.UpdatePassword in
    [ Field.(CurrentPassword |> show), [ password ]
    ; Field.(NewPassword |> show), [ new_password ]
    ; Field.(PasswordConfirmation |> show), [ new_password ]
    ]
    |> decode
    >>= handle ~notification Contact.(contact |> id |> Id.to_user)
  in
  let expected =
    Error Error.(Conformist [ Field.NewPassword, PasswordPolicyMinLength 8 ])
  in
  check_result expected events
;;

let update_password_wrong_confirmation () =
  let ((_, password, _, _, _) as contact_info) =
    "john@gmail.com" |> contact_info
  in
  let contact = contact_info |> create_contact true in
  let new_password = "Password1?" in
  let confirmed_password = "Password1*" in
  let notification = confirmation_mail contact in
  let events =
    let open Cqrs_command.User_command.UpdatePassword in
    [ Field.(CurrentPassword |> show), [ password ]
    ; Field.(NewPassword |> show), [ new_password ]
    ; Field.(PasswordConfirmation |> show), [ confirmed_password ]
    ]
    |> decode
    |> Pool_common.Utils.get_or_failwith
    |> handle ~notification Contact.(contact |> id |> Id.to_user)
  in
  let expected = Error Error.PasswordConfirmationDoesNotMatch in
  check_result expected events
;;

let request_email_validation () =
  let contact_info = "john@gmail.com" |> contact_info in
  let contact = contact_info |> create_contact true in
  let new_email =
    "john.doe@gmail.com"
    |> Pool_user.EmailAddress.create
    |> Pool_common.Utils.get_or_failwith
  in
  let token = Email.Token.create "testtoken" in
  let verification_email = verification_email contact_info in
  let allowed_email_suffixes =
    [ "gmail.com" ]
    |> CCList.map Settings.EmailSuffix.create
    |> CCResult.flatten_l
    |> CCResult.get_exn
  in
  let events =
    Contact_command.RequestEmailValidation.(
      handle ~allowed_email_suffixes token verification_email contact new_email)
  in
  let expected =
    Ok
      [ Email.Created (new_email, token, Contact.(id contact |> Id.to_user))
        |> Pool_event.email_verification
      ; Email.sent verification_email |> Pool_event.email
      ]
  in
  check_result expected events
;;

let request_email_validation_wrong_suffix () =
  let contact_info = "john@gmail.com" |> contact_info in
  let contact = contact_info |> create_contact true in
  let new_email =
    "john.doe@gmx.ch"
    |> Pool_user.EmailAddress.create
    |> Pool_common.Utils.get_or_failwith
  in
  let token = Email.Token.create "testtoken" in
  let verification_email = verification_email contact_info in
  let allowed_email_suffixes =
    [ "gmail.com" ]
    |> CCList.map Settings.EmailSuffix.create
    |> CCResult.flatten_l
    |> CCResult.get_exn
  in
  let events =
    Contact_command.RequestEmailValidation.(
      handle ~allowed_email_suffixes token verification_email contact new_email)
  in
  let expected =
    Error
      (Error.InvalidEmailSuffix
         (allowed_email_suffixes |> CCList.map Settings.EmailSuffix.value))
  in
  check_result expected events
;;

let update_email () =
  let contact = "john@gmail.com" |> contact_info |> create_contact true in
  let new_email = "john.doe@gmail.com" in
  let email_unverified =
    Email.Unverified
      { Email.address = new_email |> Pool_user.EmailAddress.of_string
      ; user = contact.Contact.user
      ; token = Email.Token.create "testing"
      ; created_at = Pool_common.CreatedAt.create_now ()
      ; updated_at = Pool_common.UpdatedAt.create_now ()
      }
  in
  let events =
    let open CCResult in
    let open Cqrs_command.User_command in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    email_unverified
    |> UpdateEmail.handle ~allowed_email_suffixes (Contact contact)
  in
  let expected =
    Ok
      [ Contact.EmailUpdated (contact, Email.address email_unverified)
        |> Pool_event.contact
      ; Email.EmailVerified email_unverified |> Pool_event.email_verification
      ]
  in
  check_result expected events
;;

let verify_email () =
  let email_address = "john@gmail.com" in
  let contact = email_address |> contact_info |> create_contact true in
  let email_unverified =
    Email.Unverified
      { Email.address = email_address |> Pool_user.EmailAddress.of_string
      ; user = contact.Contact.user
      ; token = Email.Token.create "testing"
      ; created_at = Pool_common.CreatedAt.create_now ()
      ; updated_at = Pool_common.UpdatedAt.create_now ()
      }
  in
  let events =
    let open Cqrs_command.User_command in
    email_unverified |> VerifyEmail.handle (Contact contact)
  in
  let expected =
    Ok
      [ Contact.EmailVerified contact |> Pool_event.contact
      ; Email.EmailVerified email_unverified |> Pool_event.email_verification
      ]
  in
  check_result expected events
;;

let toggle_verified () =
  let open Contact in
  let create_verified = Pool_user.Verified.create_now in
  let contact = "john@gmail.com" |> contact_info |> create_contact true in
  let run_test contact expected =
    let open Cqrs_command.Contact_command.ToggleVerified in
    let events = handle contact in
    let expected = Ok [ expected |> Pool_event.contact ] in
    check_result expected events
  in
  run_test
    contact
    (Updated { contact with verified = Some (create_verified ()) });
  run_test
    { contact with verified = Some (create_verified ()) }
    (Updated { contact with verified = None });
  ()
;;

let accept_terms_and_conditions () =
  let contact = "john@gmail.com" |> contact_info |> create_contact true in
  let events = Contact_command.AcceptTermsAndConditions.handle contact in
  let expected = Ok [ Contact.TermsAccepted contact |> Pool_event.contact ] in
  check_result expected events
;;

let should_not_send_registration_notification _ () =
  let database_label = Test_utils.Data.database_label in
  let%lwt () =
    let contact_data = Test_seed.Contacts.create_contact 12 |> CCList.pure in
    let%lwt () =
      Test_seed.Contacts.create ~contact_data Test_utils.Data.database_label
    in
    let%lwt contact = Test_seed.Contacts.find_contact_by_id database_label 12 in
    let%lwt () =
      Sihl_email.
        { sender = "test@econ.uzh.ch"
        ; recipient =
            Contact.email_address contact |> Pool_user.EmailAddress.value
        ; subject = "subject"
        ; text = ""
        ; html = Some "text"
        ; cc = []
        ; bcc = []
        }
      |> Email.(Service.Job.create %> create_dispatch)
      |> Cqrs_command.Contact_command.SendRegistrationAttemptNotifitacion.handle
           contact
      |> Test_utils.get_or_failwith
      |> Pool_event.handle_events database_label current_user
    in
    let%lwt res =
      Contact.should_send_registration_attempt_notification
        database_label
        contact
    in
    let expected = false in
    Alcotest.(check bool "succeeds" expected res) |> Lwt.return
  in
  Lwt.return_unit
;;
