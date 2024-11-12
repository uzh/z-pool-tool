module TemplateCommand = Cqrs_command.Message_template_command
open CCFun.Infix
open Pool_message

let current_user = Test_utils.Model.create_admin ()

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
  let expected = Error (Error.Invalid Field.Language) in
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
  let expected = Error (Error.CannotBeDeleted Field.MessageTemplate) in
  Test_utils.check_result expected events
;;

(* Integration tests *)

let create_experiment () =
  let database_label = Test_utils.Data.database_label in
  let experiment = Test_utils.Model.create_experiment () in
  let%lwt () =
    [ Experiment.Created experiment |> Pool_event.experiment ]
    |> Pool_event.handle_events database_label current_user
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
    |> Pool_event.handle_events database_label current_user
  in
  Lwt.return template
;;

module LanguageTestsData = struct
  open Pool_common
  open Message_template

  let database_label = Test_utils.Data.database_label
  let invitation_label = Label.ExperimentInvitation

  let create_experiment ?language () =
    { (Test_utils.Model.create_experiment ()) with Experiment.language }
  ;;

  let contact_de = Test_utils.Model.create_contact ~language:Language.De ()
  let contact_en = Test_utils.Model.create_contact ~language:Language.En ()
  let en = Pool_common.Language.En
  let de = Pool_common.Language.De

  let find_message_template experiment language label =
    find_by_label_and_language_to_send
      database_label
      ~entity_uuids:Experiment.[ experiment.Experiment.id |> Id.to_common ]
      language
      label
  ;;

  let experiment_message_language =
    experiment_message_language Pool_common.Language.all
  ;;

  let find_default_by_label_and_language lang label =
    find_default_by_label_and_language database_label lang label
  ;;

  let create_experiment_message_template experiment label language =
    let message_template =
      let entity_uuid = Experiment.(experiment.Experiment.id |> Id.to_common) in
      Test_utils.Model.create_message_template ~label ~language ~entity_uuid ()
    in
    let%lwt () = Created message_template |> handle_event database_label in
    Lwt.return message_template
  ;;

  let find_template_to_send experiment contact =
    let language = experiment_message_language experiment contact in
    find_message_template experiment invitation_label language
  ;;

  let check_template = Alcotest.(check Test_utils.message_template "succeeds")
end

let get_template_without_experiment_language_and_templates _ () =
  let open LanguageTestsData in
  let label = invitation_label in
  let experiment = create_experiment () in
  let find_template = find_template_to_send experiment in
  let%lwt default_template_de = find_default_by_label_and_language de label in
  let%lwt default_template_en = find_default_by_label_and_language en label in
  let%lwt res_de = find_template contact_de in
  let%lwt res_en = find_template contact_en in
  (* As no experiment language is defined, always expect a template in the
     contat language to be returned *)
  check_template default_template_de res_de;
  check_template default_template_en res_en;
  Lwt.return_unit
;;

let get_template_without_experiment_language _ () =
  let open LanguageTestsData in
  let label = invitation_label in
  let experiment = create_experiment () in
  let find_template = find_template_to_send experiment in
  let%lwt template_de =
    create_experiment_message_template experiment label de
  in
  let%lwt default_template_en = find_default_by_label_and_language en label in
  let%lwt res_de = find_template contact_de in
  let%lwt res_en = find_template contact_en in
  (* Expect the custom de template and the default en tempalte to be returned *)
  check_template template_de res_de;
  check_template default_template_en res_en;
  Lwt.return_unit
;;

let get_template_with_experiment_language _ () =
  let open LanguageTestsData in
  let label = invitation_label in
  let experiment = create_experiment ~language:Pool_common.Language.De () in
  let find_template = find_template_to_send experiment in
  let%lwt default_template_de = find_default_by_label_and_language de label in
  let%lwt res_de = find_template contact_de in
  let%lwt res_en = find_template contact_en in
  let check = check_template default_template_de in
  check res_de;
  check res_en;
  Lwt.return_unit
;;

let get_template_with_experiment_language_and_template _ () =
  let open LanguageTestsData in
  let label = invitation_label in
  let experiment = create_experiment ~language:Pool_common.Language.De () in
  let%lwt template_de =
    create_experiment_message_template experiment label de
  in
  let find_template = find_template_to_send experiment in
  let%lwt res_de = find_template contact_de in
  let%lwt res_en = find_template contact_en in
  let check = check_template template_de in
  check res_de;
  check res_en;
  Lwt.return_unit
;;

let get_template_with_language_missing _ () =
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
      |> Lwt_list.map_s
           (Message_template.find_by_label_and_language_to_send
              database_label
              ~entity_uuids:Experiment.[ experiment.id |> Id.to_common ]
              label)
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
      |> Lwt_list.map_s
           (Message_template.find_by_label_and_language_to_send
              database_label
              ~entity_uuids:Experiment.[ experiment.id |> Id.to_common ]
              label)
    in
    (* Expect all created templates to be returned *)
    Alcotest.(check (list Test_utils.message_template) "succeeds" templates res)
    |> Lwt.return
  in
  Lwt.return_unit
;;

module ExperimentSenderData = struct
  let experiment_id = Experiment.Id.create ()
  let admin_id = Admin.Id.create ()
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let database_label = Test_utils.Data.database_label

  let admin_email =
    Format.asprintf "admin+%s@econ.uzh.ch" Pool_common.Id.(create () |> value)
  ;;

  let get_exn = Test_utils.get_or_failwith

  let initialize () =
    let open Integration_utils in
    let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
    let%lwt (_ : Contact.t) = ContactRepo.create ~id:contact_id () in
    let open Experiment in
    Updated
      ( experiment
      , { experiment with
          contact_email = Some (Pool_user.EmailAddress.of_string admin_email)
        } )
    |> handle_event database_label
  ;;
end

let sender_of_job =
  Email.job %> Email.Service.Job.email %> fun email -> email.Sihl_email.sender
;;

let experiment_invitation_with_sender _ () =
  let open Utils.Lwt_result.Infix in
  let open ExperimentSenderData in
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
      ; mailing = None
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
        ; Pool_event.Email (Email.BulkSent [ job ])
        ; Pool_event.Contact _
        ] -> sender_of_job job
      | _ -> failwith "Event missmatch"
    in
    Alcotest.(check string "succeeds" admin_email res);
    Pool_event.handle_events database_label current_user events
  in
  Lwt.return_unit
;;

let assignment_creation_with_sender _ () =
  let open Utils.Lwt_result.Infix in
  let open ExperimentSenderData in
  let%lwt () =
    let%lwt tenant = Pool_tenant.find_by_label database_label ||> get_exn in
    let%lwt contact = Contact.find database_label contact_id ||> get_exn in
    let%lwt experiment =
      Experiment.find database_label experiment_id ||> get_exn
    in
    let%lwt session =
      Integration_utils.SessionRepo.create ~id:session_id experiment ()
    in
    let%lwt confirmation_email =
      Message_template.AssignmentConfirmation.prepare
        tenant
        contact
        experiment
        session
      ||> fun fnc -> fnc (Assignment.create contact)
    in
    Alcotest.(
      check string "succeeds" admin_email (sender_of_job confirmation_email))
    |> Lwt.return
  in
  Lwt.return_unit
;;
