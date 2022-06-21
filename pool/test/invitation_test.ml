module InvitationCommand = Cqrs_command.Invitation_command
module Field = Pool_common.Message.Field

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
  let events =
    let command =
      InvitationCommand.Create.
        { experiment; contacts = [ contact ]; invited_contacts = [] }
    in
    InvitationCommand.Create.handle command Pool_common.Language.En
  in
  let expected =
    Ok
      [ Invitation.(Created { experiment; contact }, Pool_common.Language.En)
        |> Pool_event.invitation
      ]
  in
  check_result expected events
;;

let resend () =
  let invitation = create_invitation () in
  let experiment = Test_utils.create_experiment () in
  let resent = Invitation.{ invitation; experiment } in
  let events = InvitationCommand.Resend.handle resent Pool_common.Language.En in
  let expected =
    Ok
      [ Invitation.(Resent resent, Pool_common.Language.En)
        |> Pool_event.invitation
      ]
  in
  check_result expected events
;;
