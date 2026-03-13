open CCFun
open Utils.Lwt_result.Infix
open Integration_utils
module InvitationCommand = Cqrs_command.Invitation_command
module Field = Pool_message.Field
module Model = Test_utils.Model
module JobHistory = Message_template.History

let limit = 10
let get_or_failwith = Test_utils.get_or_failwith
let pool = Test_utils.Data.database_label
let sort_events = Test_utils.sort_events
let current_user () = Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
let invitation_mail = Message_template.ExperimentInvitation.prepare
let matcher_notification_mail = Message_template.MatcherNotification.create

let create_contact_ids n_ids =
  let rec aux acc n = if n <= 0 then acc else aux (Contact.Id.create () :: acc) (n - 1) in
  aux [] n_ids
;;

let create_no_match_found_events experiment =
  let open Experiment in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let%lwt emails =
    find_admins_to_notify_about_invitations pool experiment.id
    >|> Lwt_list.map_s
          (Message_template.MatcherNotification.create
             tenant
             Pool_common.Language.En
             experiment)
    ||> Email.bulksent
    ||> Pool_event.email
  in
  let updated =
    Updated
      ( experiment
      , { experiment with
          matcher_notification_sent = MatcherNotificationSent.create true
        } )
    |> Pool_event.experiment
  in
  Lwt.return [ emails; updated ]
;;

let expected_events ?(ids = []) experiment mailing contacts create_message =
  let invitations =
    CCList.mapi
      (fun i contact -> Invitation.create ?id:(CCList.nth_opt ids i) contact)
      contacts
  in
  let emails = CCList.map create_message invitations |> CCResult.(flatten_l %> get_exn) in
  let events =
    [ Invitation.(Created { invitations; mailing; experiment }) |> Pool_event.invitation
    ; Email.BulkSent emails |> Pool_event.email
    ]
    @ CCList.map
        Contact.(update_num_invitations ~step:1 %> updated %> Pool_event.contact)
        contacts
    |> sort_events
  in
  Ok events
;;

let create_message ?sender invitation =
  let sender =
    sender |> CCOption.map_or ~default:"it@econ.uzh.ch" Pool_user.EmailAddress.value
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
         Pool_queue.(
           job_ctx_create
             JobHistory.
               [ contact_item invitation.Invitation.contact; invitation_item invitation ])
  |> CCResult.return
;;

module MatcherTestUtils = struct
  let contact_name_filter name =
    let open Filter in
    let value = Single (Str name) in
    Pred (Predicate.create Key.(Hardcoded Name) Operator.(Equality Equality.Equal) value)
    |> create None
  ;;

  let store_filter current_user experiment new_filter =
    let%lwt () =
      [ Filter.Created new_filter |> Pool_event.filter
      ; Experiment.(Updated (experiment, { experiment with filter = Some new_filter }))
        |> Pool_event.experiment
      ]
      |> Pool_event.handle_events pool current_user
    in
    Experiment.find pool experiment.Experiment.id ||> get_or_failwith
  ;;

  let setup_experiment_filter_with_matching_contacts current_user experiment contact_ids =
    let%lwt experiment =
      Experiment.Id.value experiment.Experiment.id
      |> contact_name_filter
      |> store_filter current_user experiment
    in
    let%lwt contacts =
      Lwt_list.map_s
        (fun id ->
           let lastname =
             Experiment.Id.value experiment.Experiment.id |> Pool_user.Lastname.of_string
           in
           ContactRepo.create ~id ~lastname ())
        contact_ids
      ||> Matcher.sort_contacts
    in
    Lwt.return (experiment, contacts)
  ;;

  let setup_with_contacts ?id ?(title_suffix = "test") ?(n_contacts = 4) current_user =
    let contact_ids = create_contact_ids n_contacts in
    let%lwt experiment =
      let title = Format.asprintf "Matcher experiment (%s)" title_suffix in
      ExperimentRepo.create ?id ~title ()
    in
    let%lwt experiment, contacts =
      setup_experiment_filter_with_matching_contacts current_user experiment contact_ids
    in
    Lwt.return (experiment, contacts)
  ;;

  let setup_with_invitations ?id ?title_suffix ?n_contacts current_user =
    let%lwt experiment, contacts =
      setup_with_contacts ?id ?title_suffix ?n_contacts current_user
    in
    let%lwt () =
      let open InvitationCommand.Create in
      { experiment; mailing = None; contacts; invited_contacts = []; create_message }
      |> handle
      |> Lwt_result.lift
      |>> Pool_event.handle_events pool current_user
      ||> get_or_failwith
    in
    Lwt.return (experiment, contacts)
  ;;
end

let create_invitations_model () =
  let open InvitationCommand.Create in
  let mailing = Model.create_mailing () in
  let experiment = Model.create_experiment () in
  let ids = [ Pool_common.Id.create (); Pool_common.Id.create () ] in
  let contacts = Model.[ create_contact (); create_contact () ] in
  let events =
    { mailing = Some mailing
    ; experiment
    ; contacts
    ; create_message
    ; invited_contacts = []
    }
    |> handle ~ids
  in
  let[@warning "-4"] created, emails, contacts =
    match events with
    | Ok
        [ Pool_event.Invitation
            (Invitation.Created ({ Invitation.mailing = Some _; _ } as created))
        ; Pool_event.Email (Email.BulkSent emails)
        ; Pool_event.Contact contactOne
        ; Pool_event.Contact contactTwo
        ] -> created, emails, [ contactOne; contactTwo ]
    | Ok _ -> failwith "Event mismatch"
    | Error err -> failwith Pool_common.(Utils.error_to_string Language.En err)
  in
  let open Alcotest in
  let () =
    check
      bool
      "experiment id"
      true
      (Experiment.Id.equal
         created.Invitation.experiment.Experiment.id
         experiment.Experiment.id)
  in
  let () =
    check
      bool
      "mailing id"
      true
      (Mailing.Id.equal
         (CCOption.get_exn_or "Missing mailing" created.Invitation.mailing).Mailing.id
         mailing.Mailing.id)
  in
  let () = check int "invitations" 2 (CCList.length created.Invitation.invitations) in
  check int "emails" (CCList.length contacts) (CCList.length emails)
;;

let create_invitations _ () =
  let open Mailing in
  let%lwt current_user = current_user () in
  let%lwt ({ Experiment.id; _ } as experiment), _ =
    MatcherTestUtils.setup_with_contacts ~title_suffix:"create invitations" current_user
  in
  let find_invitation_count { Experiment.id; _ } =
    Invitation.find_by_experiment pool id ||> fst ||> CCList.length
  in
  let%lwt (_ : Session.t) = Integration_utils.SessionRepo.create experiment () in
  let limit = Limit.create 100 |> get_or_failwith in
  let interval = 2 * 60 |> Ptime.Span.of_int_s in
  let%lwt (_ : Mailing.t) =
    let distribution =
      Distribution.(Sorted [ SortableField.Firstname, SortOrder.Ascending ])
    in
    Integration_utils.MailingRepo.create ~start:Start.StartNow ~distribution ~limit id
  in
  let run_matcher () = Matcher.create_invitation_events interval pool in
  let%lwt current = Status.find_current pool interval in
  let () = Alcotest.(check int "count mailings" 1 (CCList.length current)) in
  let mailing = current |> CCList.hd |> fun { Status.mailing; _ } -> mailing in
  let%lwt events = run_matcher () in
  let%lwt before = find_invitation_count experiment in
  let%lwt () = Pool_event.handle_events pool current_user events in
  let%lwt after = find_invitation_count experiment in
  let () =
    (* NOTE: Status.find_current uses "ceil" for calculating the current "to_handle" amount *)
    let limit = per_interval interval mailing |> CCFloat.(ceil %> to_int) in
    let is_less_or_equal = after - before <= limit in
    let msg =
      Format.asprintf
        "count generated invitations (%d - %d <= %d) -> smaller or equal limit"
        after
        before
        limit
    in
    Alcotest.(check bool msg true is_less_or_equal)
  in
  let%lwt () = Experiment.handle_event pool (Experiment.ResetInvitations experiment) in
  let%lwt events = run_matcher () in
  let%lwt () = Pool_event.handle_events pool current_user events in
  let%lwt after_reset = find_invitation_count experiment in
  let () =
    let msg =
      Format.asprintf
        "count generated invitations (%d == %d) -> equal to before reset"
        after
        after_reset
    in
    Alcotest.(check int msg after after_reset)
  in
  (* Stop mailing for upcoming tests - and wait for a second, as mailings refer to timestamps *)
  let%lwt () = Stopped mailing |> Mailing.handle_event pool in
  Unix.sleep 1;
  Lwt.return_unit
;;

let update_contact current_user update contact =
  let updated = update contact in
  let%lwt () =
    Contact.Updated updated
    |> Pool_event.contact
    |> Pool_event.handle_event pool current_user
  in
  Lwt.return updated
;;

let created_invitation_contact_ids events =
  let[@warning "-4"] invitation_contact_ids = function
    | Pool_event.Invitation (Invitation.Created created) ->
      created.Invitation.invitations
      |> CCList.map (fun invitation ->
        invitation.Invitation.contact |> Contact.id |> Contact.Id.value)
      |> CCOption.return
    | _ -> None
  in
  events
  |> CCList.filter_map invitation_contact_ids
  |> CCList.flatten
  |> CCList.sort CCString.compare
;;

let create_invitations_for_different_user_types _ () =
  let open MatcherTestUtils in
  let%lwt current_user = current_user () in
  let%lwt experiment, contacts =
    setup_with_contacts
      ~title_suffix:"create_invitations_different_user_types"
      ~n_contacts:4
      current_user
  in
  let open_contact, pending_import_contact, paused_contact, disabled_inactive_contact =
    match contacts with
    | [ open_contact; pending_import_contact; paused_contact; disabled_inactive_contact ]
      -> open_contact, pending_import_contact, paused_contact, disabled_inactive_contact
    | _ -> failwith "Expected exactly 4 contacts"
  in
  let%lwt pending_import_contact =
    update_contact
      current_user
      (fun contact ->
         Contact.{ contact with import_pending = Pool_user.ImportPending.create true })
      pending_import_contact
  in
  let%lwt (_ : Contact.t) =
    update_contact
      current_user
      (fun contact -> Contact.{ contact with paused = Pool_user.Paused.create true })
      paused_contact
  in
  let%lwt (_ : Contact.t) =
    update_contact
      current_user
      (fun contact ->
         Contact.
           { contact with
             disabled = Pool_user.Disabled.create true
           ; user = Pool_user.{ contact.user with status = Status.Inactive }
           })
      disabled_inactive_contact
  in
  let%lwt mailing = experiment |> Experiment.id |> MailingRepo.create in
  let%lwt events = Matcher.events_of_mailings pool [ mailing, limit ] in
  let expected_ids =
    [ open_contact; pending_import_contact ]
    |> CCList.map Contact.(id %> Id.value)
    |> CCList.sort CCString.compare
  in
  let invitation_contact_ids = created_invitation_contact_ids events in
  let () =
    Alcotest.(
      check
        (list string)
        "only open and pending import contacts receive invitations"
        expected_ids
        invitation_contact_ids)
  in
  let%lwt () = Mailing.Stopped mailing |> Mailing.handle_event pool in
  Unix.sleep 1;
  Lwt.return_unit
;;

let create_invitations_exclude_paused_and_disabled_inactive _ () =
  let open MatcherTestUtils in
  let%lwt current_user = current_user () in
  let%lwt experiment, contacts =
    setup_with_contacts
      ~title_suffix:"create_invitations_exclude_paused_and_disabled"
      ~n_contacts:2
      current_user
  in
  let paused_contact, disabled_inactive_contact =
    match contacts with
    | [ paused_contact; disabled_inactive_contact ] ->
      paused_contact, disabled_inactive_contact
    | _ -> failwith "Expected exactly 2 contacts"
  in
  let%lwt (_ : Contact.t) =
    update_contact
      current_user
      (fun contact -> Contact.{ contact with paused = Pool_user.Paused.create true })
      paused_contact
  in
  let%lwt (_ : Contact.t) =
    update_contact
      current_user
      (fun contact ->
         Contact.
           { contact with
             disabled = Pool_user.Disabled.create true
           ; user = Pool_user.{ contact.user with status = Status.Inactive }
           })
      disabled_inactive_contact
  in
  let%lwt mailing = experiment |> Experiment.id |> MailingRepo.create in
  let%lwt events = Matcher.events_of_mailings pool [ mailing, limit ] in
  let invitation_contact_ids = created_invitation_contact_ids events in
  Alcotest.(
    check
      (list string)
      "paused and disabled/inactive contacts do not receive invitations"
      []
      invitation_contact_ids);
  let%lwt () = Mailing.Stopped mailing |> Mailing.handle_event pool in
  Unix.sleep 1;
  Lwt.return_unit
;;

let create_invitations_exclude_inactive_only _ () =
  let open MatcherTestUtils in
  let%lwt current_user = current_user () in
  let%lwt experiment, contacts =
    setup_with_contacts
      ~title_suffix:"create_invitations_exclude_inactive_only"
      ~n_contacts:1
      current_user
  in
  let inactive_contact =
    match contacts with
    | [ inactive_contact ] -> inactive_contact
    | _ -> failwith "Expected exactly 1 contact"
  in
  let%lwt (_ : Contact.t) =
    update_contact
      current_user
      (fun contact ->
         Contact.
           { contact with
             user = Pool_user.{ contact.user with status = Status.Inactive }
           })
      inactive_contact
  in
  let%lwt mailing = experiment |> Experiment.id |> MailingRepo.create in
  let%lwt events = Matcher.events_of_mailings pool [ mailing, limit ] in
  let invitation_contact_ids = created_invitation_contact_ids events in
  Alcotest.(
    check
      (list string)
      "inactive-only contacts do not receive invitations"
      []
      invitation_contact_ids);
  let%lwt () = Mailing.Stopped mailing |> Mailing.handle_event pool in
  Unix.sleep 1;
  Lwt.return_unit
;;

let expected_create_events
      ?(invitation_ids = [])
      contacts
      mailing
      experiment
      invitation_mail
  =
  let open CCList in
  let invitations =
    mapi (fun i -> Invitation.create ?id:(nth_opt invitation_ids i)) contacts
  in
  let emails = invitations >|= CCFun.(invitation_mail %> get_or_failwith) in
  [ Invitation.(Created { invitations; mailing = Some mailing; experiment })
    |> Pool_event.invitation
  ; Email.BulkSent emails |> Pool_event.email
  ]
  @ map
      (fun contact ->
         Contact.(Updated (update_num_invitations ~step:1 contact)) |> Pool_event.contact)
      contacts
;;

let expected_resend_events contacts mailing experiment invitation_mail =
  let%lwt invitations =
    Lwt_list.map_s
      (fun contact ->
         contact
         |> Contact.id
         |> Invitation.find_by_contact_and_experiment_opt pool experiment.Experiment.id
         ||> CCOption.to_result (Pool_message.Error.NotFound Field.Invitation)
         ||> get_or_failwith)
      contacts
  in
  invitations
  |> CCList.flat_map (fun invitation ->
    [ Invitation.Resent (invitation, Some mailing.Mailing.id) |> Pool_event.invitation
    ; Email.sent (invitation_mail invitation |> get_or_failwith) |> Pool_event.email
    ])
  |> Lwt.return
;;

let send_invitations _ () =
  let open MatcherTestUtils in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let%lwt current_user = current_user () in
  let%lwt experiment, contacts =
    setup_with_contacts ~title_suffix:"send_invitations" current_user
  in
  let invitation_ids = CCList.map (fun _ -> Pool_common.Id.create ()) contacts in
  let%lwt mailing = experiment |> Experiment.id |> MailingRepo.create in
  let%lwt events = Matcher.events_of_mailings ~invitation_ids pool [ mailing, limit ] in
  let%lwt expected =
    let%lwt create_email = invitation_mail tenant experiment in
    expected_create_events ~invitation_ids contacts mailing experiment create_email
    |> Lwt.return
  in
  let () = Alcotest.(check (list Test_utils.event) "succeeds" expected events) in
  let%lwt () = Pool_event.handle_events pool current_user expected in
  Lwt.return_unit
;;

let reset_invitations _ () =
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let%lwt current_user = current_user () in
  let%lwt experiment, contacts =
    MatcherTestUtils.setup_with_invitations ~title_suffix:"reset_invitations" current_user
  in
  let%lwt () = Experiment.(handle_event pool (ResetInvitations experiment)) in
  let%lwt mailing = experiment |> Experiment.id |> MailingRepo.create in
  let%lwt events = Matcher.events_of_mailings pool [ mailing, limit ] in
  let%lwt expected =
    let contacts = Matcher.sort_contacts contacts in
    let%lwt create_email = invitation_mail tenant experiment in
    expected_resend_events contacts mailing experiment create_email
  in
  let () = Alcotest.(check (list Test_utils.event) "succeeds" expected events) in
  let%lwt () = Pool_event.handle_events pool current_user events in
  Lwt.return_unit
;;

let matcher_notification _ () =
  let open MatcherTestUtils in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let%lwt current_user = current_user () in
  let%lwt experiment, _ =
    setup_with_contacts ~title_suffix:"matcher_notification" current_user
  in
  let%lwt experiment =
    "that name surely does not exist"
    |> contact_name_filter
    |> store_filter current_user experiment
  in
  let email_event () =
    Experiment.find_admins_to_notify_about_invitations pool experiment.Experiment.id
    >|> Lwt_list.map_s
          (Message_template.MatcherNotification.create
             tenant
             Pool_common.Language.En
             experiment)
    ||> Email.bulksent
    ||> Pool_event.email
  in
  let%lwt mailing = experiment |> Experiment.id |> MailingRepo.create in
  let matcher_events () = Matcher.events_of_mailings pool [ mailing, limit ] in
  let%lwt events = matcher_events () in
  let%lwt expected =
    let updated =
      Experiment.
        { experiment with
          matcher_notification_sent = MatcherNotificationSent.create true
        }
    in
    let experiment = Experiment.Updated (experiment, updated) |> Pool_event.experiment in
    let%lwt emails = email_event () in
    Lwt.return [ experiment; emails ]
  in
  (* Expect notification to be sent *)
  let () = Alcotest.(check (list Test_utils.event) "succeeds" expected events) in
  let%lwt () = Pool_event.handle_events pool current_user events in
  (* Expect notification not to be sent again *)
  let%lwt events = matcher_events () in
  let () = Alcotest.(check (list Test_utils.event) "succeeds" [] events) in
  Lwt.return_unit
;;

let create_invitations_for_online_experiment _ () =
  let open MatcherTestUtils in
  let%lwt current_user = current_user () in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  let contact_ids = [ Contact.Id.create () ] in
  let invitation_ids = CCList.map (fun _ -> Pool_common.Id.create ()) contact_ids in
  let%lwt experiment =
    ExperimentRepo.create
      ~online_experiment:Experiment_test.Data.online_experiment
      ~title:"Online matcher experiment"
      ()
  in
  let%lwt experiment, contacts =
    setup_experiment_filter_with_matching_contacts current_user experiment contact_ids
  in
  let%lwt mailing =
    let start = Mailing.Start.StartNow in
    MailingRepo.create ~start experiment.Experiment.id
  in
  let run_test expected message =
    let%lwt events =
      Matcher.create_invitation_events ~invitation_ids (Ptime.Span.of_int_s (5 * 60)) pool
    in
    let%lwt expected =
      match expected with
      | `Empty -> Lwt.return []
      | `Events ->
        let%lwt create_email = invitation_mail tenant experiment in
        expected_create_events ~invitation_ids contacts mailing experiment create_email
        |> Lwt.return
    in
    Alcotest.(check (list Test_utils.event) message expected events) |> Lwt.return
  in
  (* No time window *)
  let%lwt () = run_test `Empty "no time window exists" in
  let duration = Session.Duration.create Test_utils.Time.two_hours |> get_or_failwith in
  (* Future time window *)
  let%lwt upcoming =
    let start = Model.in_two_hours () in
    TimeWindowRepo.create start duration experiment ()
  in
  let%lwt () = run_test `Events "future time window exists" in
  let%lwt () =
    let open Pool_event in
    Time_window.Deleted upcoming |> time_window |> handle_event pool current_user
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
    |> handle_event pool current_user
  in
  let%lwt () = run_test `Empty "current time window without any open spots" in
  Lwt.return_unit
;;
