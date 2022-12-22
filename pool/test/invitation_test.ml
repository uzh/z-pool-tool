module InvitationCommand = Cqrs_command.Invitation_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let create_invitation () =
  let contact = Model.create_contact () in
  Invitation.
    { id = Pool_common.Id.create ()
    ; contact
    ; resent_at = None
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let create () =
  let experiment = Model.create_experiment () in
  let contact = Model.create_contact () in
  let events =
    let command =
      InvitationCommand.Create.
        { experiment
        ; contacts = [ contact ]
        ; invited_contacts = []
        ; create_message = Matcher_test.create_message
        }
    in
    InvitationCommand.Create.handle command
  in
  let expected =
    let email =
      Matcher_test.create_message experiment contact |> CCResult.get_exn
    in
    Ok
      [ Invitation.(Created ([ contact ], experiment)) |> Pool_event.invitation
      ; Email.BulkSent [ email ] |> Pool_event.email
      ; Contact.NumInvitationsIncreased contact |> Pool_event.contact
      ]
  in
  Test_utils.check_result expected events
;;

let resend () =
  let open InvitationCommand.Resend in
  let tenant = Tenant_test.Data.full_tenant |> CCResult.get_exn in
  let invitation = create_invitation () in
  let experiment = Model.create_experiment () in
  let languages = Pool_common.Language.all in
  let i18n_templates = Test_utils.i18n_templates languages in
  let events =
    handle { invitation; experiment } tenant languages i18n_templates
  in
  let expected =
    let open CCResult in
    let* email =
      InvitationCommand.invitation_template_elements
        tenant
        languages
        i18n_templates
        experiment
        invitation.Invitation.contact.Contact.language
    in
    Ok
      [ Invitation.(Resent invitation) |> Pool_event.invitation
      ; Email.InvitationSent
          ( invitation.Invitation.contact.Contact.user
          , Invitation.email_experiment_elements experiment
          , email )
        |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;
