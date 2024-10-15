open CCFun
module InvitationCommand = Cqrs_command.Invitation_command
module Field = Pool_message.Field
module Model = Test_utils.Model

let get_or_failwith = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label
let sort_events = Test_utils.sort_events

let expected_events experiment mailing contacts create_message =
  let emails =
    CCList.map create_message contacts |> CCResult.(flatten_l %> get_exn)
  in
  let events =
    [ Invitation.(Created { contacts; mailing; experiment })
      |> Pool_event.invitation
    ; Email.BulkSent emails |> Pool_event.email
    ]
    @ CCList.map
        Contact.(
          update_num_invitations ~step:1 %> updated %> Pool_event.contact)
        contacts
    |> sort_events
  in
  Ok events
;;

let create_message ?sender (contact : Contact.t) =
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
  |> Email.Service.Job.create
  |> Email.create_dispatch
       ~job_ctx:
         Pool_queue.(job_ctx_create [ Contact.(contact |> id |> Id.to_common) ])
  |> CCResult.return
;;

let create_invitations_model () =
  let open InvitationCommand.Create in
  let mailing = Model.create_mailing () in
  let experiment = Model.create_experiment () in
  let contacts = Model.[ create_contact (); create_contact () ] in
  let events =
    { mailing = Some mailing
    ; experiment
    ; contacts
    ; create_message
    ; invited_contacts = []
    }
    |> handle
    |> CCResult.map sort_events
  in
  let expected =
    expected_events experiment (Some mailing) contacts create_message
  in
  Test_utils.check_result expected events
;;

let create_invitations_repo _ () =
  let open Utils.Lwt_result.Infix in
  let pool = Test_utils.Data.database_label in
  let find_invitation_count { Experiment.id; _ } =
    Invitation.find_by_experiment pool id ||> fst ||> CCList.length
  in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let%lwt { Experiment.id; _ } = Test_utils.Repo.first_experiment () in
  let limit = Mailing.Limit.create 100 |> get_or_failwith in
  let mailing =
    let open Mailing in
    let distribution =
      Some
        Distribution.(Sorted [ SortableField.Firstname, SortOrder.Ascending ])
    in
    create
      Start.StartNow
      (Ptime_clock.now ()
       |> flip Ptime.add_span (Ptime.Span.of_int_s 3600)
       |> CCOption.get_exn_or "Invalid end time of mailing"
       |> EndAt.create
       |> get_or_failwith)
      limit
      distribution
    |> get_or_failwith
  in
  let%lwt () = Mailing.(Created (mailing, id) |> handle_event pool) in
  let interval = 2 * 60 |> Ptime.Span.of_int_s in
  let create_message experiment =
    Message_template.ExperimentInvitation.prepare tenant experiment
  in
  let find_by_mailing mailing =
    Matcher.find_contacts_by_mailing
      pool
      mailing
      (Mailing.per_interval interval mailing |> CCFloat.(round %> to_int))
    ||> CCResult.get_exn
  in
  let find_events () =
    Matcher.create_invitation_events interval [ pool ]
    ||> CCList.assoc ~eq:Database.Label.equal pool
    ||> sort_events
  in
  let create_expected mailing experiment contacts =
    create_message experiment
    ||> expected_events experiment (Some mailing) contacts
  in
  Mailing.Status.find_current pool interval
  ||> tap (CCList.length %> Alcotest.(check int "count mailings" 1))
  >|> Lwt_list.iter_s (fun { Mailing.Status.mailing; _ } ->
    let%lwt experiment, contacts, _ = find_by_mailing mailing in
    let%lwt expected = create_expected mailing experiment contacts in
    let%lwt events = find_events () in
    let () = Test_utils.check_result expected (Ok events) in
    let%lwt before = find_invitation_count experiment in
    let%lwt () = Pool_event.handle_events pool events in
    let%lwt after = find_invitation_count experiment in
    let () =
      let msg = "count generated invitations -> smaller or equal limit" in
      let is_less_or_equal =
        after - before
        <= (Mailing.per_interval interval mailing |> CCFloat.(round %> to_int))
      in
      Alcotest.(check bool msg true is_less_or_equal)
    in
    let%lwt () =
      Experiment.handle_event pool (Experiment.ResetInvitations experiment)
    in
    let%lwt experiment, contacts, _ = find_by_mailing mailing in
    let%lwt expected = create_expected mailing experiment contacts in
    let%lwt events = find_events () in
    let () = Test_utils.check_result expected (Ok events) in
    let%lwt () = Pool_event.handle_events pool events in
    let%lwt after_reset = find_invitation_count experiment in
    let () =
      let msg = "count generated invitations -> equal to before reset" in
      Alcotest.(check int msg after after_reset)
    in
    Lwt.return_unit)
;;

open Integration_utils
open Utils.Lwt_result.Infix

let expected_create_events contacts mailing experiment invitation_mail =
  let emails =
    contacts |> CCList.map CCFun.(invitation_mail %> get_or_failwith)
  in
  [ Invitation.(Created { contacts; mailing = Some mailing; experiment })
    |> Pool_event.invitation
  ; Email.BulkSent emails |> Pool_event.email
  ]
  @ CCList.map
      (fun contact ->
        Contact.(Updated (update_num_invitations ~step:1 contact))
        |> Pool_event.contact)
      contacts
;;

let expected_resend_events contacts mailing experiment invitation_mail =
  let%lwt invitations =
    Lwt_list.map_s
      (fun contact ->
        contact
        |> Contact.id
        |> Invitation.find_by_contact_and_experiment_opt
             database_label
             experiment.Experiment.id
        ||> CCOption.to_result (Pool_message.Error.NotFound Field.Invitation)
        ||> get_or_failwith)
      contacts
  in
  invitations
  |> CCList.flat_map (fun invitation ->
    [ Invitation.Resent (invitation, Some mailing.Mailing.id)
      |> Pool_event.invitation
    ; Email.sent
        (invitation_mail invitation.Invitation.contact |> get_or_failwith)
      |> Pool_event.email
    ])
  |> Lwt.return
;;

let experiment_id = Experiment.Id.create ()
let contact_ids = Contact.Id.[ create (); create (); create (); create () ]
let invitation_mail = Message_template.ExperimentInvitation.prepare
let matcher_notification_mail = Message_template.MatcherNotification.create

module MatcherTestUtils = struct
  let contact_name_filter name =
    let open Filter in
    let value = Single (Str name) in
    Pred
      (Predicate.create
         Key.(Hardcoded Name)
         Operator.(Equality Equality.Equal)
         value)
    |> create None
  ;;

  let store_filter experiment new_filter =
    let%lwt () =
      [ Filter.Created new_filter |> Pool_event.filter
      ; Experiment.(Updated { experiment with filter = Some new_filter })
        |> Pool_event.experiment
      ]
      |> Pool_event.handle_events database_label
    in
    Experiment.find database_label experiment.Experiment.id ||> get_or_failwith
  ;;

  let setup_experiment_filter_with_matching_contacts experiment contact_ids =
    let%lwt experiment =
      Experiment.Id.value experiment.Experiment.id
      |> contact_name_filter
      |> store_filter experiment
    in
    let%lwt contacts =
      Lwt_list.map_s
        (fun id ->
          let lastname =
            Experiment.Id.value experiment.Experiment.id
            |> Pool_user.Lastname.of_string
          in
          ContactRepo.create ~id ~lastname ())
        contact_ids
      ||> Matcher.sort_contacts
    in
    Lwt.return (experiment, contacts)
  ;;
end

let limit = 10

let create_invitations _ () =
  let open MatcherTestUtils in
  let%lwt tenant =
    Pool_tenant.find_by_label database_label ||> get_or_failwith
  in
  let%lwt experiment =
    ExperimentRepo.create ~id:experiment_id ~title:"Matcher experiment" ()
  in
  let%lwt experiment, contacts =
    setup_experiment_filter_with_matching_contacts experiment contact_ids
  in
  let%lwt mailing = MailingRepo.create experiment_id in
  let%lwt events =
    Matcher.events_of_mailings [ database_label, [ mailing, limit ] ]
    ||> CCList.hd
    ||> snd
  in
  let%lwt expected =
    let%lwt create_email = invitation_mail tenant experiment in
    expected_create_events contacts mailing experiment create_email
    |> Lwt.return
  in
  let () =
    Alcotest.(check (list Test_utils.event) "succeeds" expected events)
  in
  let%lwt () = Pool_event.handle_events database_label expected in
  Lwt.return_unit
;;

let reset_invitations _ () =
  let%lwt tenant =
    Pool_tenant.find_by_label database_label ||> get_or_failwith
  in
  let%lwt experiment =
    let open Experiment in
    let invitation_reset_at =
      Ptime.sub_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600)
      |> CCOption.map InvitationResetAt.of_ptime
    in
    find database_label experiment_id
    ||> get_or_failwith
    ||> fun experiment -> { experiment with invitation_reset_at }
  in
  let%lwt () = Experiment.(Updated experiment |> handle_event database_label) in
  let%lwt contacts =
    Lwt_list.map_s
      (fun id -> Contact.find database_label id ||> get_or_failwith)
      contact_ids
    ||> Matcher.sort_contacts
  in
  let%lwt mailing = MailingRepo.create experiment_id in
  let%lwt events =
    Matcher.events_of_mailings [ database_label, [ mailing, limit ] ]
    ||> CCList.hd
    ||> snd
  in
  let%lwt expected =
    let%lwt create_email = invitation_mail tenant experiment in
    expected_resend_events contacts mailing experiment create_email
  in
  let () =
    Alcotest.(check (list Test_utils.event) "succeeds" expected events)
  in
  let%lwt () = Pool_event.handle_events database_label events in
  Lwt.return_unit
;;

let matcher_notification _ () =
  let open MatcherTestUtils in
  let%lwt tenant =
    Pool_tenant.find_by_label database_label ||> get_or_failwith
  in
  let%lwt experiment =
    Experiment.find database_label experiment_id ||> get_or_failwith
  in
  let%lwt experiment =
    "that name surely does not exist"
    |> contact_name_filter
    |> store_filter experiment
  in
  let email_event () =
    Experiment.find_admins_to_notify_about_invitations
      database_label
      experiment.Experiment.id
    >|> Lwt_list.map_s (fun admin ->
      admin
      |> Message_template.MatcherNotification.create
           tenant
           Pool_common.Language.En
           experiment)
    ||> Email.bulksent
    ||> Pool_event.email
  in
  let%lwt mailing = MailingRepo.create experiment_id in
  let matcher_events () =
    Matcher.events_of_mailings [ database_label, [ mailing, limit ] ]
  in
  let%lwt events = matcher_events () ||> CCList.hd ||> snd in
  let%lwt expected =
    let experiment =
      Experiment.(
        Updated
          { experiment with
            matcher_notification_sent = MatcherNotificationSent.create true
          })
      |> Pool_event.experiment
    in
    let%lwt emails = email_event () in
    Lwt.return [ emails; experiment ]
  in
  (* Expect notification to be sent *)
  let () =
    Alcotest.(check (list Test_utils.event) "succeeds" expected events)
  in
  let%lwt () = Pool_event.handle_events database_label events in
  (* Expect notification not to be sent again *)
  let%lwt events =
    matcher_events ()
    ||> function
    | [] -> []
    | events -> events |> CCList.hd |> snd
  in
  let () = Alcotest.(check (list Test_utils.event) "succeeds" [] events) in
  Lwt.return_unit
;;

let create_invitations_for_online_experiment _ () =
  let interval = Ptime.Span.of_int_s (5 * 60) in
  let open MatcherTestUtils in
  let%lwt tenant =
    Pool_tenant.find_by_label database_label ||> get_or_failwith
  in
  let contact_ids = [ Contact.Id.create () ] in
  let%lwt experiment =
    ExperimentRepo.create
      ~online_experiment:Experiment_test.Data.online_experiment
      ~title:"Online matcher experiment"
      ()
  in
  let%lwt experiment, contacts =
    setup_experiment_filter_with_matching_contacts experiment contact_ids
  in
  let%lwt mailing =
    let start = Mailing.Start.StartNow in
    MailingRepo.create ~start experiment.Experiment.id
  in
  let run_test expected message =
    let%lwt events =
      Matcher.create_invitation_events interval [ database_label ]
      ||> CCList.assoc_opt ~eq:Database.Label.equal database_label
      ||> CCOption.value ~default:[]
    in
    let%lwt expected =
      match expected with
      | `Empty -> Lwt.return []
      | `Events ->
        let%lwt create_email = invitation_mail tenant experiment in
        expected_create_events contacts mailing experiment create_email
        |> Lwt.return
    in
    Alcotest.(check (list Test_utils.event) message expected events)
    |> Lwt.return
  in
  (* No time window *)
  let%lwt () = run_test `Empty "no time window exists" in
  let duration = Session.Duration.create Model.two_hours |> get_or_failwith in
  (* Future time window *)
  let%lwt upcoming =
    let start = Model.in_two_hours () in
    TimeWindowRepo.create start duration experiment ()
  in
  let%lwt () = run_test `Events "future time window exists" in
  let%lwt () =
    let open Pool_event in
    Time_window.Deleted upcoming |> time_window |> handle_event database_label
  in
  (* Current time window *)
  let%lwt current =
    let start = Model.an_hour_ago () in
    TimeWindowRepo.create start duration experiment ()
  in
  let%lwt () = run_test `Events "current time window exists" in
  (* Current time window without any open spots *)
  let%lwt () =
    let open Pool_event in
    let max_participants =
      Session.ParticipantAmount.create 0 |> get_or_failwith |> CCOption.return
    in
    Time_window.(Updated { current with max_participants })
    |> time_window
    |> handle_event database_label
  in
  let%lwt () = run_test `Empty "current time window without any open spots" in
  Lwt.return_unit
;;
