module MatcherCommand = Cqrs_command.Matcher_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let expected_events
  ({ Experiment.smtp_auth_id; _ } as experiment)
  contacts
  create_message
  =
  let emails =
    CCList.map create_message contacts
    |> CCResult.flatten_l
    |> CCResult.get_exn
    |> CCList.map (fun msg -> msg, smtp_auth_id)
  in
  Ok
    ([ Invitation.Created (contacts, experiment) |> Pool_event.invitation
     ; Email.BulkSent emails |> Pool_event.email
     ]
     @ CCList.map
         Contact.(
           fun contact ->
             contact
             |> update_num_invitations ~step:1
             |> updated
             |> Pool_event.contact)
         contacts)
;;

let create_message ?sender (_ : Contact.t) =
  let sender =
    sender
    |> CCOption.map_or ~default:"it@econ.uzh.ch" Pool_user.EmailAddress.value
  in
  Sihl_email.
    { sender
    ; recipient = "contact@econ.uzh.ch"
    ; subject = "Invitation"
    ; text = ""
    ; html = None
    ; cc = []
    ; bcc = []
    }
  |> CCResult.return
;;

let create_invitations_model () =
  let open Cqrs_command.Matcher_command.Run in
  let mailing = Model.create_mailing () in
  let experiment = Model.create_experiment () in
  let contacts = Model.[ create_contact (); create_contact () ] in
  let events =
    { mailing; experiment; contacts; create_message } |> CCList.pure |> handle
  in
  let expected = expected_events experiment contacts create_message in
  Test_utils.check_result expected events
;;

let create_invitations_repo _ () =
  let open Utils.Lwt_result.Infix in
  let pool = Test_utils.Data.database_label in
  let tenant = Tenant_test.Data.full_tenant |> CCResult.get_exn in
  let%lwt () =
    let create_message experiment =
      Message_template.ExperimentInvitation.prepare tenant experiment
    in
    Mailing.find_current pool
    >|> Lwt_list.iter_s (fun ({ Mailing.rate; _ } as mailing : Mailing.t) ->
      let open Cqrs_command.Matcher_command.Run in
      let%lwt experiment, contacts =
        Matcher.find_contacts_by_mailing pool mailing (Mailing.Rate.value rate)
        ||> CCResult.get_exn
      in
      let%lwt create_message = create_message experiment in
      let events =
        { mailing; experiment; contacts; create_message }
        |> CCList.pure
        |> handle
      in
      let expected =
        let emails =
          CCList.map create_message contacts
          |> CCList.all_ok
          |> CCResult.get_exn
          |> CCList.map (fun msg -> msg, experiment.Experiment.smtp_auth_id)
        in
        Ok
          ([ Invitation.Created (contacts, experiment) |> Pool_event.invitation
           ; Email.BulkSent emails |> Pool_event.email
           ]
           @ CCList.map
               Contact.(
                 fun contact ->
                   contact
                   |> update_num_invitations ~step:1
                   |> updated
                   |> Pool_event.contact)
               contacts)
      in
      Test_utils.check_result expected events |> Lwt.return)
  in
  Lwt.return_unit
;;
