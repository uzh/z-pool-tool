module Subject_command = Cqrs_command.Subject_command
module Message = Pool_common.Message
module Field = Message.Field
module Language = Pool_common.Language

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let subject_info email_address =
  ( email_address
  , "password"
  , "Jane"
  , "Doe"
  , Subject.RecruitmentChannel.(Friend |> show)
  , Some Language.En )
;;

let sign_up_subject subject_info =
  let email_address, password, firstname, lastname, recruitment_channel, _ =
    subject_info
  in
  [ Field.(Email |> show), [ email_address ]
  ; Field.(Password |> show), [ password ]
  ; Field.(Firstname |> show), [ firstname ]
  ; Field.(Lastname |> show), [ lastname ]
  ; Field.(RecruitmentChannel |> show), [ recruitment_channel ]
  ]
;;

let create_subject verified subject_info =
  let ( email_address
      , password
      , firstname
      , lastname
      , recruitment_channel
      , language )
    =
    subject_info
  in
  { Subject.user =
      Sihl_user.
        { id = Pool_common.Id.(create () |> value)
        ; email = email_address
        ; username = None
        ; name = Some lastname
        ; given_name = Some firstname
        ; password
        ; status =
            Sihl_user.status_of_string "active" |> CCResult.get_or_failwith
        ; admin = false
        ; confirmed = true
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
  ; recruitment_channel = Subject.RecruitmentChannel.read recruitment_channel
  ; terms_accepted_at = Pool_user.TermsAccepted.create_now ()
  ; language
  ; paused = Pool_user.Paused.create false
  ; disabled = Pool_user.Disabled.create false
  ; verified = Pool_user.Verified.create None
  ; email_verified =
      (if verified then Some (Ptime_clock.now ()) else None)
      |> Pool_user.EmailVerified.create
  ; num_invitations = Subject.NumberOfInvitations.init
  ; num_assignments = Subject.NumberOfAssignments.init
  ; firstname_version = Pool_common.Version.create ()
  ; lastname_version = Pool_common.Version.create ()
  ; paused_version = Pool_common.Version.create ()
  ; language_version = Pool_common.Version.create ()
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let sign_up_not_allowed_suffix () =
  let events =
    let open CCResult in
    let open Subject_command.SignUp in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    "john@bluewin.com"
    |> subject_info
    |> sign_up_subject
    |> decode
    |> Pool_common.Utils.get_or_failwith
    |> handle ~allowed_email_suffixes None
  in
  let expected = Error Message.(Invalid Field.EmailSuffix) in
  check_result expected events
;;

let sign_up () =
  let user_id = Pool_common.Id.create () in
  let terms_accepted_at = Pool_user.TermsAccepted.create_now () in
  let (( email_address
       , password
       , firstname
       , lastname
       , recruitment_channel
       , language ) as subject_info)
    =
    subject_info "john@gmail.com"
  in
  let events =
    let open CCResult in
    let open Subject_command.SignUp in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    subject_info
    |> sign_up_subject
    |> decode
    |> Pool_common.Utils.get_or_failwith
    |> handle ~allowed_email_suffixes ~user_id ~terms_accepted_at language
  in
  CCList.iter
    (fun m -> print_endline (Pool_event.show m))
    (events |> Pool_common.Utils.get_or_failwith);
  let expected =
    let email = email_address |> Pool_user.EmailAddress.of_string in
    let firstname = firstname |> Pool_user.Firstname.of_string in
    let lastname = lastname |> Pool_user.Lastname.of_string in
    let subject : Subject.create =
      { Subject.user_id
      ; email
      ; password =
          password
          |> Pool_user.Password.create
          |> Pool_common.Utils.get_or_failwith
      ; firstname
      ; lastname
      ; recruitment_channel =
          recruitment_channel |> Subject.RecruitmentChannel.read
      ; terms_accepted_at
      ; language
      }
    in
    Ok
      [ Subject.Created subject |> Pool_event.subject
      ; Email.Created
          ( email
          , user_id
          , firstname
          , lastname
          , language |> CCOption.get_exn_or "Test failed" )
        |> Pool_event.email_address
      ]
  in
  check_result expected events
;;

let delete_unverified () =
  let subject = "john@gmail.com" |> subject_info |> create_subject false in
  let events = Subject_command.DeleteUnverified.handle subject in
  let expected =
    Ok [ Subject.UnverifiedDeleted subject |> Pool_event.subject ]
  in
  check_result expected events
;;

let delete_verified () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let events = Subject_command.DeleteUnverified.handle subject in
  let expected = Error Message.EmailDeleteAlreadyVerified in
  check_result expected events
;;

let update_language () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let language = Language.De in
  let events =
    Subject_command.Update.(
      [ Field.(Language |> show), [ language |> Language.code ] ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle subject)
  in
  let expected =
    Ok [ Subject.LanguageUpdated (subject, language) |> Pool_event.subject ]
  in
  check_result expected events
;;

let update_paused () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let paused = true in
  let events =
    Subject_command.Update.(
      [ Field.(Paused |> show), [ paused |> string_of_bool ] ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle subject)
  in
  let expected =
    Ok
      [ Subject.PausedUpdated (subject, paused |> Pool_user.Paused.create)
        |> Pool_event.subject
      ]
  in
  check_result expected events
;;

let update_full () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let firstname = "Max" in
  let lastname = "Muster" in
  let paused = true in
  let language = Language.De in
  let events =
    Subject_command.Update.(
      [ Field.(Firstname |> show), [ firstname ]
      ; Field.(Lastname |> show), [ lastname ]
      ; Field.(Paused |> show), [ paused |> string_of_bool ]
      ; Field.(Language |> show), [ language |> Language.code ]
      ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle subject)
  in
  let expected =
    Ok
      (CCList.map
         Pool_event.subject
         [ Subject.FirstnameUpdated
             (subject, firstname |> Pool_user.Firstname.of_string)
         ; Subject.LastnameUpdated
             (subject, lastname |> Pool_user.Lastname.of_string)
         ; Subject.PausedUpdated (subject, paused |> Pool_user.Paused.create)
         ; Subject.LanguageUpdated (subject, language)
         ])
  in
  check_result expected events
;;

let update_password () =
  let ((_, password, _, _, _, language) as subject_info) =
    "john@gmail.com" |> subject_info
  in
  let subject = subject_info |> create_subject true in
  let new_password = "testing" in
  let events =
    Subject_command.UpdatePassword.(
      [ Field.(CurrentPassword |> show), [ password ]
      ; Field.(NewPassword |> show), [ new_password ]
      ; Field.(PasswordConfirmation |> show), [ new_password ]
      ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle ~password_policy:(CCFun.const (CCResult.pure ())) subject)
  in
  let expected =
    Ok
      [ Subject.PasswordUpdated
          ( subject
          , password
            |> Pool_user.Password.create
            |> Pool_common.Utils.get_or_failwith
          , new_password
            |> Pool_user.Password.create
            |> Pool_common.Utils.get_or_failwith
          , new_password |> Pool_user.PasswordConfirmed.create
          , language |> CCOption.get_or ~default:Language.En )
        |> Pool_event.subject
      ]
  in
  check_result expected events
;;

let update_password_wrong_current_password () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let current_password = "something else" in
  let new_password = "short" in
  let events =
    Subject_command.UpdatePassword.(
      [ Field.(CurrentPassword |> show), [ current_password ]
      ; Field.(NewPassword |> show), [ new_password ]
      ; Field.(PasswordConfirmation |> show), [ new_password ]
      ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle subject)
  in
  let expected = Error Message.(Invalid Field.CurrentPassword) in
  check_result expected events
;;

let update_password_wrong_policy () =
  let ((_, password, _, _, _, _) as subject_info) =
    "john@gmail.com" |> subject_info
  in
  let subject = subject_info |> create_subject true in
  let new_password = "short" in
  let events =
    Subject_command.UpdatePassword.(
      [ Field.(CurrentPassword |> show), [ password ]
      ; Field.(NewPassword |> show), [ new_password ]
      ; Field.(PasswordConfirmation |> show), [ new_password ]
      ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle subject)
  in
  let expected =
    Error Message.(PasswordPolicy I18n.Key.(PasswordPolicyText |> to_string))
  in
  check_result expected events
;;

let update_password_wrong_confirmation () =
  let ((_, password, _, _, _, _) as subject_info) =
    "john@gmail.com" |> subject_info
  in
  let subject = subject_info |> create_subject true in
  let new_password = "testing" in
  let confirmed_password = "something else" in
  let events =
    Subject_command.UpdatePassword.(
      [ Field.(CurrentPassword |> show), [ password ]
      ; Field.(NewPassword |> show), [ new_password ]
      ; Field.(PasswordConfirmation |> show), [ confirmed_password ]
      ]
      |> decode
      |> Pool_common.Utils.get_or_failwith
      |> handle ~password_policy:(CCFun.const (CCResult.pure ())) subject)
  in
  let expected = Error Pool_common.Message.PasswordConfirmationDoesNotMatch in
  check_result expected events
;;

let request_email_validation () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let new_email = "john.doe@gmail.com" in
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    Subject_command.RequestEmailValidation.(
      new_email
      |> Pool_user.EmailAddress.create
      |> Pool_common.Utils.get_or_failwith
      |> handle ~allowed_email_suffixes subject)
  in
  let expected =
    Ok
      [ Email.Updated
          ( new_email |> Pool_user.EmailAddress.of_string
          , subject.Subject.user
          , subject.Subject.language
            |> CCOption.get_or ~default:Pool_common.Language.En )
        |> Pool_event.email_address
      ]
  in
  check_result expected events
;;

let request_email_validation_wrong_suffix () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let new_email = "john.doe@gmx.com" in
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    Subject_command.RequestEmailValidation.(
      new_email
      |> Pool_user.EmailAddress.create
      |> Pool_common.Utils.get_or_failwith
      |> handle ~allowed_email_suffixes subject)
  in
  let expected = Error Message.(Invalid Field.EmailSuffix) in
  check_result expected events
;;

let update_email () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let new_email = "john.doe@gmail.com" in
  let email_unverified =
    Email.Unverified
      { Email.address = new_email |> Pool_user.EmailAddress.of_string
      ; user = subject.Subject.user
      ; token = Email.Token.create "testing"
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
  in
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map Settings.EmailSuffix.create
      |> CCResult.flatten_l
    in
    Subject_command.UpdateEmail.(
      email_unverified |> handle ~allowed_email_suffixes subject)
  in
  let expected =
    Ok
      [ Subject.EmailUpdated (subject, Email.address email_unverified)
        |> Pool_event.subject
      ; Email.EmailVerified email_unverified |> Pool_event.email_address
      ]
  in
  check_result expected events
;;

let verify_email () =
  let email_address = "john@gmail.com" in
  let subject = email_address |> subject_info |> create_subject true in
  let email_unverified =
    Email.Unverified
      { Email.address = email_address |> Pool_user.EmailAddress.of_string
      ; user = subject.Subject.user
      ; token = Email.Token.create "testing"
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
  in
  let events =
    Subject_command.VerifyEmail.({ email = email_unverified } |> handle subject)
  in
  let expected =
    Ok
      [ Subject.EmailVerified subject |> Pool_event.subject
      ; Email.EmailVerified email_unverified |> Pool_event.email_address
      ]
  in
  check_result expected events
;;

let accept_terms_and_conditions () =
  let subject = "john@gmail.com" |> subject_info |> create_subject true in
  let events = Subject_command.AcceptTermsAndConditions.handle subject in
  let expected = Ok [ Subject.TermsAccepted subject |> Pool_event.subject ] in
  check_result expected events
;;
