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
    let email = Matcher_test.create_message contact |> CCResult.get_exn in
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
  let invitation = create_invitation () in
  let experiment = Model.create_experiment () in
  let email = Test_utils.Model.create_email () in
  let events = handle email { invitation; experiment } in
  let expected =
    let open CCResult in
    Ok
      [ Invitation.(Resent invitation) |> Pool_event.invitation
      ; Email.Sent email |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;
