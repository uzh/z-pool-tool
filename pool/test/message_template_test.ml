module TemplateCommand = Cqrs_command.Message_template_command
module Field = Pool_common.Message.Field

module Data = struct
  let urlencoded =
    Field.
      [ Language |> show, [ "DE" ]
      ; EmailSubject |> show, [ "Subject" ]
      ; EmailText |> show, [ "Lorem ipsum" ]
      ; PlainText |> show, [ "Lorem ipsum" ]
      ; SmsText |> show, [ "Lorem ipsum" ]
      ]
  ;;

  let label = Message_template.Label.ExperimentInvitation

  let create ?entity_uuid id =
    let open TemplateCommand.Create in
    urlencoded
    |> decode
    |> CCResult.get_exn
    |> fun { language; email_subject; email_text; plain_text; sms_text } ->
    Message_template.
      { id
      ; label
      ; entity_uuid
      ; language
      ; email_subject
      ; email_text
      ; plain_text
      ; sms_text
      }
  ;;
end

let test_create ?id ?entity_uuid available_languages expected =
  let open TemplateCommand.Create in
  let open CCResult in
  let entity_uuid =
    entity_uuid |> CCOption.value ~default:(Pool_common.Id.create ())
  in
  let events =
    Data.urlencoded
    |> decode
    >>= handle ?id Data.label entity_uuid available_languages
  in
  Test_utils.check_result expected events
;;

let create () =
  let entity_uuid = Pool_common.Id.create () in
  let id = Message_template.Id.create () in
  let template = Data.create ~entity_uuid id in
  let available_languages = Pool_common.Language.all in
  let expected =
    Ok Message_template.[ Created template |> Pool_event.message_template ]
  in
  test_create ~id ~entity_uuid available_languages expected
;;

let create_with_unavailable_language () =
  let available_languages = Pool_common.Language.[ En ] in
  let expected = Error Pool_common.Message.(Invalid Field.Language) in
  test_create available_languages expected
;;

let delete_valid () =
  let open Message_template in
  let id = Id.create () in
  let template = Data.create ~entity_uuid:(Pool_common.Id.create ()) id in
  let events = TemplateCommand.Delete.handle template in
  let expected = Ok [ Deleted template |> Pool_event.message_template ] in
  Test_utils.check_result expected events
;;

let delete_without_entity () =
  let open Message_template in
  let id = Id.create () in
  let template = Data.create id in
  let events = TemplateCommand.Delete.handle template in
  let expected =
    Error Pool_common.Message.(CannotBeDeleted Field.MessageTemplate)
  in
  Test_utils.check_result expected events
;;

(* Integration tests *)

let create_experiment () =
  let database_label = Test_utils.Data.database_label in
  let experiment = Test_utils.Model.create_experiment () in
  let%lwt () =
    [ Experiment.Created experiment |> Pool_event.experiment ]
    |> Pool_event.handle_events database_label
  in
  Lwt.return experiment
;;

let create_invitation language ?entity_uuid () =
  let database_label = Test_utils.Data.database_label in
  let label = Message_template.Label.ExperimentInvitation in
  let template =
    Test_utils.Model.create_message_template ~label ~language ?entity_uuid ()
  in
  let%lwt () =
    [ Message_template.Created template |> Pool_event.message_template ]
    |> Pool_event.handle_events database_label
  in
  Lwt.return template
;;

let get_template_with_language_missing _ () =
  let open Utils.Lwt_result.Infix in
  let%lwt () =
    let database_label = Test_utils.Data.database_label in
    let label = Message_template.Label.ExperimentInvitation in
    let%lwt experiment = create_experiment () in
    let template_language = Pool_common.Language.En in
    let%lwt template =
      create_invitation
        template_language
        ~entity_uuid:Experiment.(experiment.id |> Id.to_common)
        ()
    in
    let%lwt res =
      Pool_common.Language.[ De; En ]
      |> Lwt_list.map_s (fun lang ->
        Message_template.find_by_label_to_send
          database_label
          ~entity_uuids:Experiment.[ experiment.id |> Id.to_common ]
          lang
          label
        ||> fst)
    in
    (* When one entity specific template exists, expect this to be returned
       every time *)
    let expected = [ template; template ] in
    Alcotest.(check (list Test_utils.message_template) "succeeds" expected res)
    |> Lwt.return
  in
  Lwt.return_unit
;;

let get_templates_in_multile_languages _ () =
  let open Utils.Lwt_result.Infix in
  let%lwt () =
    let database_label = Test_utils.Data.database_label in
    let label = Message_template.Label.ExperimentInvitation in
    let%lwt experiment = create_experiment () in
    let languages = Pool_common.Language.[ De; En ] in
    let%lwt templates =
      languages
      |> Lwt_list.map_s (fun lang ->
        create_invitation
          lang
          ~entity_uuid:Experiment.(experiment.id |> Id.to_common)
          ())
    in
    let%lwt res =
      languages
      |> Lwt_list.map_s (fun lang ->
        Message_template.find_by_label_to_send
          database_label
          ~entity_uuids:Experiment.[ experiment.id |> Id.to_common ]
          lang
          label
        ||> fst)
    in
    (* Expect all created templates to be returned *)
    Alcotest.(check (list Test_utils.message_template) "succeeds" templates res)
    |> Lwt.return
  in
  Lwt.return_unit
;;

module MessageTemplateData = struct
  let experiment_id = Experiment.Id.create ()
  let admin_id = Admin.Id.create ()
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let database_label = Test_utils.Data.database_label

  let admin_email =
    Format.asprintf "admin+%s@econ.uzh.ch" (Uuidm.v `V4 |> Uuidm.to_string)
  ;;

  let get_exn = Test_utils.get_or_failwith_pool_error

  let initialize () =
    let open Integration_utils in
    let%lwt admin = AdminRepo.create ~id:admin_id ~email:admin_email () in
    let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
    let%lwt (_ : Contact.t) = ContactRepo.create ~id:contact_id () in
    let open Experiment in
    Updated { experiment with contact_person_id = Some (Admin.id admin) }
    |> handle_event database_label
  ;;
end

let experiment_invitation_with_sender _ () =
  let open Utils.Lwt_result.Infix in
  let open MessageTemplateData in
  let%lwt () =
    let%lwt () = initialize () in
    let%lwt tenant = Pool_tenant.find_by_label database_label ||> get_exn in
    let%lwt experiment =
      Experiment.find database_label experiment_id ||> get_exn
    in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt create_message =
      Message_template.ExperimentInvitation.prepare tenant experiment
    in
    let events =
      let open Cqrs_command.Invitation_command.Create in
      { experiment
      ; contacts = [ contact ]
      ; invited_contacts = []
      ; create_message
      }
      |> handle
      |> get_exn
    in
    let[@warning "-4"] res =
      match events with
      | [ Pool_event.Invitation _
        ; Pool_event.Email (Email.BulkSent [ (email, _) ])
        ; Pool_event.Contact _
        ] -> email.Sihl_email.sender
      | _ -> failwith "Event missmatch"
    in
    Alcotest.(check string "succeeds" admin_email res);
    events |> Pool_event.handle_events database_label
  in
  Lwt.return_unit
;;

let assignment_creation_with_sender _ () =
  let open Utils.Lwt_result.Infix in
  let open MessageTemplateData in
  let%lwt () =
    let%lwt tenant = Pool_tenant.find_by_label database_label ||> get_exn in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt experiment =
      Experiment.find database_label experiment_id ||> get_exn
    in
    let%lwt session =
      Integration_utils.SessionRepo.create ~id:session_id experiment_id ()
    in
    let%lwt admin = Admin.find database_label admin_id ||> get_exn in
    let%lwt confirmation_email =
      let%lwt language = Contact.message_language database_label contact in
      Message_template.AssignmentConfirmation.create
        database_label
        language
        tenant
        experiment
        session
        contact
        (Some admin)
    in
    Alcotest.(
      check string "succeeds" admin_email confirmation_email.Sihl_email.sender)
    |> Lwt.return
  in
  Lwt.return_unit
;;
