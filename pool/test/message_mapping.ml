open CCFun.Infix
open Utils.Lwt_result.Infix
open Integration_utils
open Message_template
open Pool_queue.Status

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
  let%lwt (_ : Assignment.t) = AssignmentRepo.create ~id:assignment_id session contact in
  Lwt.return_unit
;;

let find_experiment () = Experiment.find database_label experiment_id ||> get_exn
let find_contact () = Contact.find database_label contact_id ||> get_exn
let find_session () = Session.find database_label session_id ||> get_exn
let find_assignment () = Assignment.find database_label assignment_id ||> get_exn
let sort_entity_uuids = CCList.stable_sort Pool_common.Id.compare

let check_text_message =
  let text_message_testable = Text_message.(Alcotest.testable pp_job equal_job) in
  Alcotest.(check text_message_testable "succeeds")
;;

let check_message_template ?label =
  let open Alcotest in
  let valid_label = Message_template.Label.(Alcotest.testable pp equal) in
  Email.message_template
  %> CCOption.map (Message_template.Label.of_string %> Pool_common.Utils.get_or_failwith)
  %> check (option valid_label) "correct message template" label
;;

let check_mapped_uuids expected =
  let open Alcotest in
  let valid_id = Pool_common.Id.(Alcotest.testable pp equal) in
  Email.job_ctx
  %> function
  | Some (Pool_queue.Create mapping_uuids) ->
    check
      (list valid_id)
      "correct mapped ids"
      (expected |> CCList.stable_sort Pool_common.Id.compare)
      mapping_uuids
  | Some (Pool_queue.Clone _) | None ->
    Alcotest.fail "Invalid mapping ids, received clone event or none"
;;

let account_suspension_notification _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    AccountSuspensionNotification.create tenant (Contact.user contact) ||> get_exn
  in
  let expected =
    Label.AccountSuspensionNotification, [ Contact.Id.to_common contact_id ]
  in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let assignment_confirmation _ () =
  let%lwt contact = find_contact () in
  let%lwt experiment = find_experiment () in
  let%lwt session = find_session () in
  let%lwt assignment = find_assignment () in
  let%lwt res =
    AssignmentConfirmation.prepare tenant contact experiment session
    ||> fun create -> create assignment
  in
  let expected =
    ( Label.AssignmentConfirmation
    , [ Experiment.Id.to_common experiment_id
      ; Session.Id.to_common session_id
      ; Contact.Id.to_common contact_id
      ] )
  in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let assignment_session_change _ () =
  let%lwt contact = find_contact () in
  let%lwt experiment = find_experiment () in
  let%lwt session = find_session () in
  let%lwt new_session = SessionRepo.create experiment () in
  let%lwt assignment = find_assignment () in
  let message =
    Test_utils.Model.create_manual_message ~recipient:(Contact.email_address contact) ()
  in
  let%lwt res =
    AssignmentSessionChange.create
      message
      tenant
      experiment
      ~new_session
      ~old_session:session
      assignment
  in
  let expected =
    ( Label.AssignmentSessionChange
    , [ Experiment.Id.to_common experiment_id
      ; Session.Id.to_common session_id
      ; Session.(Id.to_common new_session.id)
      ; Contact.Id.to_common contact_id
      ] )
  in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let contact_email_change_attempt _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    ContactEmailChangeAttempt.create tenant (Contact.user contact) ||> get_exn
  in
  let expected = Label.ContactEmailChangeAttempt, [ Contact.Id.to_common contact_id ] in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let contact_registration_attempt _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    ContactRegistrationAttempt.create language tenant (Contact.user contact)
  in
  let expected = Label.ContactRegistrationAttempt, [ Contact.Id.to_common contact_id ] in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
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
  in
  let expected = Label.EmailVerification, [ Contact.Id.to_common contact_id ] in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let experiment_invitation _ () =
  let%lwt contact = find_contact () in
  let%lwt experiment = find_experiment () in
  let%lwt res = ExperimentInvitation.create tenant experiment contact in
  let expected =
    ( Label.ExperimentInvitation
    , [ Experiment.Id.to_common experiment_id; Contact.Id.to_common contact_id ] )
  in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let password_change _ () =
  let%lwt contact = find_contact () in
  let%lwt res = PasswordChange.create language tenant (Contact.user contact) in
  let expected = Label.PasswordChange, [ Contact.Id.to_common contact_id ] in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
  Lwt.return_unit
;;

let password_reset _ () =
  let%lwt contact = find_contact () in
  let%lwt res =
    PasswordReset.create database_label language (Tenant tenant) (Contact.user contact)
    ||> get_exn
  in
  let expected = Label.PasswordReset, [ Contact.Id.to_common contact_id ] in
  let () = check_message_template ~label:(fst expected) res in
  let () = check_mapped_uuids (snd expected) res in
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
  in
  let expected =
    Text_message.create_job
      ~message_template:Label.(show PhoneVerification)
      ~job_ctx:(Pool_queue.job_ctx_create [ Contact.Id.to_common contact_id ])
      (res |> Text_message.job)
  in
  let () = check_text_message expected res in
  Lwt.return_unit
;;

let session_reminder _ () =
  let%lwt experiment = find_experiment () in
  let%lwt session = find_session () in
  let%lwt assignment = find_assignment () in
  let cell_phone = "+41791234567" |> Pool_user.CellPhone.of_string in
  let%lwt email_res =
    SessionReminder.prepare_emails database_label tenant [ language ] experiment session
    ||> fun msg -> msg assignment |> get_exn
  in
  let%lwt text_msg_res =
    SessionReminder.prepare_text_messages
      database_label
      tenant
      [ language ]
      experiment
      session
    ||> fun msg -> msg assignment cell_phone |> get_exn
  in
  let expected_label = Label.SessionReminder in
  let expected_uuids =
    [ Experiment.Id.to_common experiment_id
    ; Session.Id.to_common session_id
    ; Contact.Id.to_common contact_id
    ]
  in
  let expected =
    Text_message.create_job
      ~message_template:Label.(show expected_label)
      ~job_ctx:(Pool_queue.job_ctx_create expected_uuids)
      (text_msg_res |> Text_message.job)
  in
  let () = check_message_template ~label:expected_label email_res in
  let () = check_mapped_uuids expected_uuids email_res in
  let () = check_text_message expected text_msg_res in
  Lwt.return_unit
;;

module Resend = struct
  module Command = Cqrs_command.Queue_command
  module Model = Test_utils.Model

  let cell_phone = "+41791234567" |> Pool_user.CellPhone.of_string

  let to_queue_job ?(status = Succeeded) =
    Pool_queue.Instance.create ~max_tries:10 ~status database_label
  ;;

  let email_queue_job ?status () =
    Model.create_email_job ()
    |> Email.(job %> Service.Job.encode)
    |> to_queue_job ?status Pool_queue.JobName.SendEmail
  ;;

  let text_message_queue_job ?status () =
    Model.create_text_message_job cell_phone
    |> Text_message.(job %> Service.Job.encode)
    |> to_queue_job ?status Pool_queue.JobName.SendTextMessage
  ;;

  let test events expected =
    Alcotest.(
      check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
  ;;

  let resend_pending () =
    let email_job = email_queue_job ~status:Pending () in
    let events = Command.Resend.handle email_job in
    let expected = Error Pool_message.Error.JobPending in
    Alcotest.(
      check (result (list Test_utils.event) Test_utils.error) "succeeds" expected events)
  ;;

  let resend_unchanged () =
    let email_job = email_queue_job () in
    let events = Command.Resend.handle email_job in
    let expected =
      Ok
        [ Model.create_email_job
            ~job_ctx:Pool_queue.(job_ctx_clone (email_job |> Instance.id))
            ()
          |> Email.sent
          |> Pool_event.email
        ]
    in
    test events expected;
    let text_message_job = text_message_queue_job () in
    let events = Command.Resend.handle text_message_job in
    let expected =
      Ok
        [ Model.create_text_message_job
            ~job_ctx:Pool_queue.(Clone (text_message_job |> Instance.id))
            cell_phone
          |> Text_message.sent
          |> Pool_event.text_message
        ]
    in
    test events expected
  ;;

  let resend_updated_recipient () =
    let open Pool_user in
    let email_job = email_queue_job () in
    let new_cellphone = CellPhone.of_string "+41799999999" in
    let new_email_address = EmailAddress.of_string "updated@email.com" in
    let contact =
      let open Contact in
      let contact = Model.create_contact () in
      let pool_user = { (user contact) with Pool_user.email = new_email_address } in
      { contact with cell_phone = Some new_cellphone; user = pool_user }
    in
    let events = Command.Resend.handle ~contact email_job in
    let expected =
      Ok
        [ Model.create_email_job
            ~job_ctx:(Pool_queue.Clone (email_job |> Pool_queue.Instance.id))
            ()
          |> Email.sent ~new_email_address
          |> Pool_event.email
        ]
    in
    test events expected;
    let text_message_job = text_message_queue_job () in
    let events = Command.Resend.handle ~contact text_message_job in
    let expected =
      Ok
        [ Model.create_text_message_job
            ~job_ctx:Pool_queue.(Clone (text_message_job |> Instance.id))
            cell_phone
          |> Text_message.sent ~new_recipient:new_cellphone
          |> Pool_event.text_message
        ]
    in
    test events expected
  ;;

  let resend_updated_smtp () =
    let email_job = email_queue_job () in
    let new_smtp_auth_id = Email.SmtpAuth.Id.create () in
    let experiment =
      Experiment.
        { (Model.create_experiment ()) with smtp_auth_id = Some new_smtp_auth_id }
    in
    let events = Command.Resend.handle ~experiment email_job in
    let expected =
      Ok
        [ Model.create_email_job
            ~job_ctx:(Pool_queue.Clone (email_job |> Pool_queue.Instance.id))
            ()
          |> Email.sent ~new_smtp_auth_id
          |> Pool_event.email
        ]
    in
    test events expected
  ;;
end
