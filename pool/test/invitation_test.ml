module InvitationCommand = Cqrs_command.Invitation_command
module Field = Pool_common.Message.Field

let i18n_templates languages =
  let open I18n in
  let open CCResult in
  CCList.map
    (fun language ->
      let subject =
        "Subject"
        |> Content.create
        >|= create Key.InvitationSubject language
        |> get_exn
      in
      let text =
        "Text"
        |> Content.create
        >|= create Key.InvitationText language
        |> get_exn
      in
      language, (subject, text))
    languages
;;

let check_result expected generated =
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      generated)
;;

let create_invitation () =
  let contact = Test_utils.create_contact () in
  Invitation.
    { id = Pool_common.Id.create ()
    ; contact
    ; resent_at = None
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let create () =
  let experiment = Test_utils.create_experiment () in
  let contact = Test_utils.create_contact () in
  let languages = Pool_common.Language.all in
  let i18n_templates = i18n_templates languages in
  let events =
    let command =
      InvitationCommand.Create.
        { experiment; contacts = [ contact ]; invited_contacts = [] }
    in
    InvitationCommand.Create.handle command languages i18n_templates
  in
  let expected =
    let email =
      let open Pool_common.Language in
      let subject, text = CCList.assoc ~eq:equal En i18n_templates in
      ( contact
      , Email.CustomTemplate.
          { subject = Subject.I18n subject; content = Content.I18n text } )
      |> CCList.pure
    in
    Ok
      [ Invitation.(Created { experiment; contact }) |> Pool_event.invitation
      ; Invitation.InvitationsSent (experiment, email) |> Pool_event.invitation
      ]
  in
  check_result expected events
;;

let resend () =
  let invitation = create_invitation () in
  let experiment = Test_utils.create_experiment () in
  let languages = Pool_common.Language.all in
  let i18n_templates = i18n_templates languages in
  let resent = Invitation.{ invitation; experiment } in
  let events =
    InvitationCommand.Resend.handle resent languages i18n_templates
  in
  let expected =
    let open CCResult in
    let* email =
      InvitationCommand.invitation_template_elements
        languages
        i18n_templates
        experiment
        invitation.Invitation.contact.Contact.language
    in
    Ok [ Invitation.(Resent (resent, email)) |> Pool_event.invitation ]
  in
  check_result expected events
;;
