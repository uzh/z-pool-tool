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

  let create id entity_uuid =
    let open TemplateCommand.Create in
    urlencoded
    |> decode
    |> CCResult.get_exn
    |> fun { language; email_subject; email_text; plain_text; sms_text } ->
    Message_template.
      { id
      ; label
      ; entity_uuid = Some entity_uuid
      ; language
      ; email_subject
      ; email_text
      ; plain_text
      ; sms_text
      }
  ;;
end

let test_create ?id ?entity_id available_languages expected =
  let open TemplateCommand.Create in
  let open CCResult in
  let entity_id =
    entity_id |> CCOption.value ~default:(Pool_common.Id.create ())
  in
  let events =
    Data.urlencoded
    |> decode
    >>= handle ?id Data.label entity_id available_languages
  in
  Test_utils.check_result expected events
;;

let create () =
  let entity_id = Pool_common.Id.create () in
  let id = Message_template.Id.create () in
  let template = Data.create id entity_id in
  let available_languages = Pool_common.Language.all in
  let expected =
    Ok Message_template.[ Created template |> Pool_event.message_template ]
  in
  test_create ~id ~entity_id available_languages expected
;;

let create_with_unavailable_language () =
  let available_languages = Pool_common.Language.[ En ] in
  let expected = Error Pool_common.Message.(Invalid Field.Language) in
  test_create available_languages expected
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
           |> Lwt.map CCResult.get_exn
           |> Lwt.map fst)
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
      |> Lwt_list.map_s (fun lang ->
           Message_template.find_by_label_to_send
             database_label
             ~entity_uuids:Experiment.[ experiment.id |> Id.to_common ]
             lang
             label
           |> Lwt.map CCResult.get_exn
           |> Lwt.map fst)
    in
    (* Expect all created templates to be returned *)
    Alcotest.(check (list Test_utils.message_template) "succeeds" templates res)
    |> Lwt.return
  in
  Lwt.return_unit
;;
