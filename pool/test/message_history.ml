module History = Queue.History
open Utils.Lwt_result.Infix
open Integration_utils
open Message_template

let get_exn = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label
let tenant = Tenant_test.Data.full_tenant |> get_exn
let experiment_id = Experiment.Id.create ()
let contact_id = Contact.Id.create ()
let session_id = Session.Id.create ()
let assignment_id = Assignment.Id.create ()
let language = Pool_common.Language.En

let initialize _ () =
  let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
  let%lwt contact = ContactRepo.create ~id:contact_id () in
  let%lwt session = SessionRepo.create ~id:session_id experiment () in
  let%lwt (_ : Assignment.t) =
    AssignmentRepo.create ~id:assignment_id session contact
  in
  Lwt.return_unit
;;

let find_experiment () =
  Experiment.find database_label experiment_id ||> get_exn
;;

let find_contact () = Contact.find database_label contact_id ||> get_exn
let find_session () = Session.find database_label session_id ||> get_exn

let find_assignment () =
  Assignment.find database_label assignment_id ||> get_exn
;;

let sort_entity_uuids = CCList.sort Pool_common.Id.compare

let sort_history history =
  History.
    { history with
      entity_uuids = CCList.sort Pool_common.Id.compare history.entity_uuids
    }
;;

let check_history_create =
  Alcotest.(check (option Test_utils.message_history_crate) "succeeds")
;;

let with_sorted_entity_uuids =
  let open CCFun.Infix in
  Email.job_message_history %> CCOption.map sort_history
;;

let txt_msg_with_sorted_entity_uuids =
  let open CCFun.Infix in
  Text_message.job_message_history %> CCOption.map sort_history
;;

let create_history label entity_uuids =
  Some
    History.
      { message_template = Some Label.(show label)
      ; entity_uuids = sort_entity_uuids entity_uuids
      }
;;

let account_suspension_notification _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    AccountSuspensionNotification.create tenant (Contact.user contact)
    ||> get_exn
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ]
    |> create_history Label.AccountSuspensionNotification
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let assignment_confirmation _ () =
  let%lwt contact = find_contact () in
  let%lwt experiment = find_experiment () in
  let%lwt session = find_session () in
  let%lwt assignment = find_assignment () in
  let%lwt res =
    AssignmentConfirmation.prepare tenant contact experiment session None
    ||> fun create -> create assignment |> with_sorted_entity_uuids
  in
  let expected =
    [ Experiment.Id.to_common experiment_id
    ; Session.Id.to_common session_id
    ; Contact.Id.to_common contact_id
    ]
    |> create_history Label.AssignmentConfirmation
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let assignment_session_change _ () =
  let%lwt contact = find_contact () in
  let%lwt experiment = find_experiment () in
  let%lwt session = find_session () in
  let%lwt new_session = SessionRepo.create experiment () in
  let%lwt assignment = find_assignment () in
  let message =
    Test_utils.Model.create_manual_message
      ~recipient:(Contact.email_address contact)
      ()
  in
  let%lwt res =
    AssignmentSessionChange.create
      message
      tenant
      experiment
      ~new_session
      ~old_session:session
      assignment
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Experiment.Id.to_common experiment_id
    ; Session.Id.to_common session_id
    ; Session.(Id.to_common new_session.id)
    ; Contact.Id.to_common contact_id
    ]
    |> create_history Label.AssignmentSessionChange
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let contact_email_change_attempt _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    ContactEmailChangeAttempt.create tenant (Contact.user contact)
    ||> get_exn
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ]
    |> create_history Label.ContactEmailChangeAttempt
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let contact_registration_attempt _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    ContactRegistrationAttempt.create language tenant (Contact.user contact)
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ]
    |> create_history Label.ContactRegistrationAttempt
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let email_verification _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    EmailVerification.create
      database_label
      language
      (Tenant tenant)
      contact
      ("new@email.com" |> Pool_user.EmailAddress.of_string)
      ("123123123" |> Email.Token.create)
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ]
    |> create_history Label.EmailVerification
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let experiment_invitation _ () =
  let%lwt contact = find_contact () in
  let%lwt experiment = find_experiment () in
  let%lwt res =
    ExperimentInvitation.create tenant experiment contact
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Experiment.Id.to_common experiment_id; Contact.Id.to_common contact_id ]
    |> create_history Label.ExperimentInvitation
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let password_change _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    PasswordChange.create language tenant (Contact.user contact)
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ] |> create_history Label.PasswordChange
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let password_reset _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    PasswordReset.create
      database_label
      language
      (Tenant tenant)
      (Contact.user contact)
    ||> get_exn
    ||> with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ] |> create_history Label.PasswordReset
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let phone_verification _ () =
  let%lwt contact = find_contact () in
  let cell_phone = "+41791234567" |> Pool_user.CellPhone.of_string in
  let token = "123123" |> Pool_common.VerificationCode.of_string in
  let%lwt res =
    PhoneVerification.create_text_message
      database_label
      language
      tenant
      contact
      cell_phone
      token
    ||> get_exn
    ||> txt_msg_with_sorted_entity_uuids
  in
  let expected =
    [ Contact.Id.to_common contact_id ]
    |> create_history Label.PhoneVerification
  in
  let () = check_history_create expected res in
  Lwt.return_unit
;;

let session_reminder _ () =
  let%lwt experiment = find_experiment () in
  let%lwt session = find_session () in
  let%lwt assignment = find_assignment () in
  let cell_phone = "+41791234567" |> Pool_user.CellPhone.of_string in
  let%lwt email_res =
    SessionReminder.prepare_emails
      database_label
      tenant
      [ language ]
      experiment
      session
    ||> fun msg -> msg assignment |> get_exn |> with_sorted_entity_uuids
  in
  let%lwt text_msg_res =
    SessionReminder.prepare_text_messages
      database_label
      tenant
      [ language ]
      experiment
      session
    ||> fun msg ->
    msg assignment cell_phone |> get_exn |> txt_msg_with_sorted_entity_uuids
  in
  let expected =
    [ Experiment.Id.to_common experiment_id
    ; Session.Id.to_common session_id
    ; Contact.Id.to_common contact_id
    ]
    |> create_history Label.SessionReminder
  in
  let () = check_history_create expected email_res in
  let () = check_history_create expected text_msg_res in
  Lwt.return_unit
;;

module Resend = struct
  module Command = Cqrs_command.Queue_command
  module Model = Test_utils.Model

  let cell_phone = "+41791234567" |> Pool_user.CellPhone.of_string

  let to_queue_job ?(status = Sihl_queue.Succeeded) name input =
    let now = Ptime_clock.now () in
    Sihl_queue.
      { id = Pool_common.Id.(create () |> value)
      ; name = Queue.JobName.show name
      ; input = Yojson.Safe.to_string input
      ; tries = 0
      ; max_tries = 10
      ; next_run_at = now
      ; status
      ; last_error = None
      ; last_error_at = None
      ; tag = None
      ; ctx = database_label |> Database.to_ctx
      }
  ;;

  let email_queue_job ?status () =
    Model.create_email_job ()
    |> Email.yojson_of_job
    |> to_queue_job ?status Queue.JobName.SendEmail
  ;;

  let text_message_queue_job ?status () =
    Model.create_text_message_job cell_phone
    |> Text_message.yojson_of_job
    |> to_queue_job ?status Queue.JobName.SendTextMessage
  ;;

  let test events expected =
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
  ;;

  let resend_pending () =
    let email_job = email_queue_job ~status:Sihl_queue.Pending () in
    let events = Command.Resend.handle email_job in
    let expected = Error Pool_message.Error.JobPending in
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
  ;;

  let resend_unchanged () =
    let email_job = email_queue_job () in
    let events = Command.Resend.handle email_job in
    let expected =
      let job =
        Email.
          { (Model.create_email_job ()) with
            resent = Some (Pool_common.Id.of_string email_job.Sihl_queue.id)
          }
      in
      Ok [ Email.(Sent job) |> Pool_event.email ]
    in
    test events expected;
    let text_message_job = text_message_queue_job () in
    let events = Command.Resend.handle text_message_job in
    let expected =
      let job =
        Text_message.
          { (Model.create_text_message_job cell_phone) with
            resent =
              Some (Pool_common.Id.of_string text_message_job.Sihl_queue.id)
          }
      in
      Ok [ Text_message.(Sent job) |> Pool_event.text_message ]
    in
    test events expected
  ;;

  let resend_updated_recipient () =
    let open Pool_user in
    let email_job = email_queue_job () in
    let updated_cellphone = CellPhone.of_string "+41799999999" in
    let updated_email_address = "updated@email.com" in
    let contact =
      let open Contact in
      let contact = Model.create_contact () in
      let sihl_user =
        Sihl_user.{ (user contact) with email = updated_email_address }
      in
      { contact with cell_phone = Some updated_cellphone; user = sihl_user }
    in
    let events = Command.Resend.handle ~contact email_job in
    let expected =
      let job =
        Email.
          { (Model.create_email_job ~recipient:updated_email_address ()) with
            resent = Some (Pool_common.Id.of_string email_job.Sihl_queue.id)
          }
      in
      Ok [ Email.(Sent job) |> Pool_event.email ]
    in
    test events expected;
    let text_message_job = text_message_queue_job () in
    let events = Command.Resend.handle ~contact text_message_job in
    let expected =
      let job =
        Text_message.
          { (Model.create_text_message_job updated_cellphone) with
            resent =
              Some (Pool_common.Id.of_string text_message_job.Sihl_queue.id)
          }
      in
      Ok [ Text_message.(Sent job) |> Pool_event.text_message ]
    in
    test events expected
  ;;

  let resend_updated_smtp () =
    let email_job = email_queue_job () in
    let updated_smtp_auth_id = Email.SmtpAuth.Id.create () in
    let experiment =
      Experiment.
        { (Model.create_experiment ()) with
          smtp_auth_id = Some updated_smtp_auth_id
        }
    in
    let events = Command.Resend.handle ~experiment email_job in
    let expected =
      let job =
        Email.
          { (Model.create_email_job ~smtp_auth_id:updated_smtp_auth_id ()) with
            resent = Some (Pool_common.Id.of_string email_job.Sihl_queue.id)
          }
      in
      Ok [ Email.(Sent job) |> Pool_event.email ]
    in
    test events expected
  ;;
end
