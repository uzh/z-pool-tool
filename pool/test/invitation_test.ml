module InvitationCommand = Cqrs_command.Invitation_command
module Field = Pool_message.Field
module Model = Test_utils.Model

let create_invitation () = Model.create_contact () |> Invitation.create

let test_creation experiment contact expected =
  let events =
    let command =
      InvitationCommand.Create.
        { experiment
        ; mailing = None
        ; contacts = [ contact ]
        ; invited_contacts = []
        ; create_message = Matcher_test.create_message
        }
    in
    InvitationCommand.Create.handle command
  in
  Test_utils.check_result expected events
;;

let create () =
  let experiment = Model.create_experiment () in
  let contact = Model.create_contact () in
  let expected =
    let email = Matcher_test.create_message contact |> CCResult.get_exn in
    let contact_update =
      let open Contact in
      contact |> update_num_invitations ~step:1 |> updated |> Pool_event.contact
    in
    Ok
      [ Invitation.(
          Created { contacts = [ contact ]; mailing = None; experiment })
        |> Pool_event.invitation
      ; Email.BulkSent [ email ] |> Pool_event.email
      ; contact_update
      ]
  in
  test_creation experiment contact expected
;;

let create_with_experiment_smtp () =
  let experiment = Model.create_experiment () in
  let contact = Model.create_contact () in
  let smtp_auth_id = Email.SmtpAuth.Id.create () in
  let experiment =
    { experiment with Experiment.smtp_auth_id = Some smtp_auth_id }
  in
  let expected =
    let email = Matcher_test.create_message contact |> CCResult.get_exn in
    let contact_update =
      let open Contact in
      contact |> update_num_invitations ~step:1 |> updated |> Pool_event.contact
    in
    Ok
      [ Invitation.(
          Created { contacts = [ contact ]; mailing = None; experiment })
        |> Pool_event.invitation
      ; Email.BulkSent [ email ] |> Pool_event.email
      ; contact_update
      ]
  in
  test_creation experiment contact expected
;;

let resend () =
  let open InvitationCommand.Resend in
  let invitation = create_invitation () in
  let experiment = Model.create_experiment () in
  let email =
    Test_utils.Model.create_email_job
      ?smtp_auth_id:experiment.Experiment.smtp_auth_id
      ()
  in
  let create_messge _ = Ok email in
  let events = handle create_messge invitation in
  let expected =
    let open CCResult in
    Ok
      [ Invitation.(Resent (invitation, None)) |> Pool_event.invitation
      ; Email.Sent email |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;
