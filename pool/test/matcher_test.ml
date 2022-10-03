module MatcherCommand = Cqrs_command.Matcher_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let expected_events experiment contacts i18n_templates =
  let emails =
    CCList.map
      (fun { Contact.user; language; _ } ->
        Cqrs_command.Invitation_command.invitation_template_elements
          Pool_common.Language.all
          i18n_templates
          experiment
          language
        |> CCResult.map (fun template ->
             ( user
             , [ ( "experimentDescription"
                 , experiment.Experiment.description
                   |> Experiment.Description.value )
               ]
             , template )))
      contacts
    |> CCList.all_ok
    |> CCResult.get_exn
  in
  Ok
    [ Invitation.Created (contacts, experiment) |> Pool_event.invitation
    ; Email.InvitationBulkSent emails |> Pool_event.email
    ]
;;

let create_invitations_model () =
  let open Cqrs_command.Matcher_command.Run in
  let mailing = Model.create_mailing () in
  let experiment = Model.create_experiment () in
  let contacts = Model.[ create_contact (); create_contact () ] in
  let languages = Pool_common.Language.all in
  let i18n_templates = Test_utils.i18n_templates languages in
  let events =
    { mailing; experiment; contacts; i18n_templates } |> CCList.pure |> handle
  in
  let expected = expected_events experiment contacts i18n_templates in
  Test_utils.check_result expected events
;;

let create_invitations_repo _ () =
  let open Utils.Lwt_result.Infix in
  let pool = Test_utils.Data.database_label in
  let%lwt () =
    Mailing.find_current pool
    >|> Lwt_list.iter_s (fun ({ Mailing.rate; _ } as mailing : Mailing.t) ->
          let open Cqrs_command.Matcher_command.Run in
          let%lwt experiment, contacts, i18n_templates =
            Matcher.find_contacts_by_mailing
              pool
              mailing
              (Mailing.Rate.value rate)
          in
          let events =
            { mailing; experiment; contacts; i18n_templates }
            |> CCList.pure
            |> handle
          in
          let expected =
            let emails =
              CCList.map
                (fun { Contact.user; language; _ } ->
                  Cqrs_command.Invitation_command.invitation_template_elements
                    Pool_common.Language.all
                    i18n_templates
                    experiment
                    language
                  |> CCResult.map (fun template ->
                       ( user
                       , [ ( "experimentDescription"
                           , experiment.Experiment.description
                             |> Experiment.Description.value )
                         ]
                       , template )))
                contacts
              |> CCList.all_ok
              |> CCResult.get_exn
            in
            Ok
              [ Invitation.Created (contacts, experiment)
                |> Pool_event.invitation
              ; Email.InvitationBulkSent emails |> Pool_event.email
              ]
          in
          Test_utils.check_result expected events |> Lwt.return)
  in
  Lwt.return_unit
;;
