module MatcherCommand = Cqrs_command.Matcher_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let expected_events experiment contacts create_message =
  let emails =
    CCList.map (fun contact -> create_message experiment contact) contacts
    |> CCResult.flatten_l
    |> CCResult.get_exn
  in
  Ok
    ([ Invitation.Created (contacts, experiment) |> Pool_event.invitation
     ; Email.BulkSent emails |> Pool_event.email
     ]
    @ CCList.map
        (fun contact ->
          Contact.NumInvitationsIncreased contact |> Pool_event.contact)
        contacts)
;;

let create_message (_ : Experiment.t) (_ : Contact.t) =
  Sihl_email.
    { sender = "it@econ.uzh.ch"
    ; recipient = "contact@econ.uzh.ch"
    ; subject = "Invitation"
    ; text = ""
    ; html = None
    ; cc = []
    ; bcc = []
    }
  |> CCResult.pure
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
    let%lwt create_message =
      Message_template.ExperimentInvitation.prepare tenant ||> CCResult.get_exn
    in
    Mailing.find_current pool
    >|> Lwt_list.iter_s (fun ({ Mailing.rate; _ } as mailing : Mailing.t) ->
          let open Cqrs_command.Matcher_command.Run in
          let%lwt experiment, contacts =
            Matcher.find_contacts_by_mailing
              pool
              mailing
              (Mailing.Rate.value rate)
            ||> CCResult.get_exn
          in
          let events =
            { mailing; experiment; contacts; create_message }
            |> CCList.pure
            |> handle
          in
          let expected =
            let emails =
              CCList.map
                (fun contact -> create_message experiment contact)
                contacts
              |> CCList.all_ok
              |> CCResult.get_exn
            in
            Ok
              ([ Invitation.Created (contacts, experiment)
                 |> Pool_event.invitation
               ; Email.BulkSent emails |> Pool_event.email
               ]
              @ CCList.map
                  (fun contact ->
                    Contact.NumInvitationsIncreased contact
                    |> Pool_event.contact)
                  contacts)
          in
          Test_utils.check_result expected events |> Lwt.return)
  in
  Lwt.return_unit
;;
