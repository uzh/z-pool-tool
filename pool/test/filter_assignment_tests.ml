let ( let@ ) = CCResult.( >>= )
let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f
let ( let& ) = Lwt_result.bind
let test_db = Test_utils.Data.database_label
let current_user = Test_utils.Model.create_admin ()

let session ~experiment =
  let open Session in
  let sid = Id.create () in
  let now = Ptime_clock.now () in
  let session_start = Start.create now in
  let* session_duration =
    let one_day = Ptime.Span.of_int_s 86_400 in
    let tomorrow =
      Ptime.add_span now one_day
      |> CCOption.get_exn_or "could not add one day to now"
    in
    Duration.create (Ptime.diff tomorrow now)
  in
  let pid = Pool_location.Id.create () in
  let pool_address = Pool_location.Address.virtual_ in
  let pool_status = Pool_location.Status.Active in
  let mapping_file = [] in
  let* pool_location =
    Pool_location.create
      ~id:pid
      "a-pool-location"
      None
      pool_address
      None
      pool_status
      mapping_file
  in
  let* max_participants = ParticipantAmount.create 2112 in
  let* min_participants = ParticipantAmount.create 1984 in
  let* overbook = ParticipantAmount.create 7 in
  let session =
    Session.create
      ~id:sid
      session_start
      session_duration
      pool_location
      max_participants
      min_participants
      overbook
      experiment
  in
  let events =
    [ Pool_location.created pool_location |> Pool_event.pool_location
    ; Session.Created session |> Pool_event.session
    ]
  in
  let& () =
    Pool_event.handle_events test_db current_user events |> Lwt_result.ok
  in
  Lwt_result.lift (Ok session)
;;

let experiment () =
  let open Experiment in
  let experiment_id = Id.create () in
  let* title = Title.create "an-experiment-title" in
  let* public_title = PublicTitle.create "a-public-title" in
  let direct_registration_disabled = DirectRegistrationDisabled.create false in
  let registration_disabled = RegistrationDisabled.create false in
  let allow_uninvited_singup = AllowUninvitedSignup.create false in
  let external_data_required = ExternalDataRequired.create false in
  let show_external_data_id_links = ShowExternalDataIdLinks.create false in
  let* experiment =
    Experiment.create
      ~id:experiment_id
      title
      public_title
      direct_registration_disabled
      registration_disabled
      allow_uninvited_singup
      external_data_required
      show_external_data_id_links
  in
  let experiment_created =
    experiment |> Experiment.created |> Pool_event.experiment
  in
  let& () =
    Pool_event.handle_event test_db current_user experiment_created
    |> Lwt_result.ok
  in
  Experiment.find test_db experiment_id
;;

let contact ~prefix () =
  let open Contact in
  let user_id = Contact.Id.create () in
  let* email =
    let email =
      Format.asprintf "%s+%s@domain.test" prefix (Contact.Id.value user_id)
    in
    Pool_user.EmailAddress.create email
  in
  let password =
    Pool_user.Password.Plain.(
      create "Somepassword1!" |> validate |> Pool_common.Utils.get_or_failwith)
  in
  let* firstname = Pool_user.Firstname.create "firstname" in
  let* lastname = Pool_user.Lastname.create "lastname" in
  let terms_accepted_at =
    Pool_user.TermsAccepted.create (Ptime_clock.now ()) |> CCOption.return
  in
  let language = Pool_common.Language.En |> CCOption.return in
  let contact_created =
    [ Contact.created
        { user_id
        ; email
        ; password
        ; firstname
        ; lastname
        ; terms_accepted_at
        ; language
        }
      |> Pool_event.contact
    ]
  in
  let& () =
    Pool_event.handle_events test_db current_user contact_created
    |> Lwt_result.ok
  in
  let& contact = Contact.find test_db user_id in
  let%lwt token = Email.create_token test_db email in
  let* verification_events =
    let open Cqrs_command.User_command in
    let created_email =
      Email.Created (email, token, user_id |> Id.to_user)
      |> Pool_event.email_verification
    in
    let email = Email.create email contact.user token in
    let@ verify_events = VerifyEmail.handle (Contact contact) email in
    Ok (created_email :: verify_events)
  in
  let& () =
    Pool_event.handle_events test_db current_user verification_events
    |> Lwt_result.ok
  in
  let& contact = Contact.find test_db user_id in
  Lwt_result.lift (Ok contact)
;;

let assignment ~experiment ~session ~contact =
  let open Cqrs_command.Assignment_command in
  let already_enrolled = false in
  let* events =
    Create.(
      handle
        { experiment; contact; follow_up_sessions = []; session }
        (fun (_ : Assignment.t) ->
           Sihl_email.create
             ~sender:"sender"
             ~recipient:"recipient"
             ~subject:"subject"
             "body"
           |> Email.Service.Job.create
           |> Email.create_dispatch)
        already_enrolled)
  in
  let& () =
    Pool_event.handle_events test_db current_user events |> Lwt_result.ok
  in
  Lwt_result.lift (Ok ())
;;

let invitation ~experiment ~contacts =
  let open Cqrs_command.Invitation_command in
  let* events =
    Create.(
      handle
        { experiment
        ; contacts
        ; invited_contacts = []
        ; create_message =
            (fun (_ : Contact.t) ->
              Sihl_email.create
                ~sender:"sender"
                ~recipient:"recipient"
                ~subject:"subject"
                "body"
              |> Email.Service.Job.create
              |> Email.create_dispatch
              |> CCResult.return)
        ; mailing = None
        })
  in
  let& () =
    Pool_event.handle_events test_db current_user events |> Lwt_result.ok
  in
  Lwt_result.lift (Ok ())
;;

(** This test verifies that given a contact that has accpeted an invitation to an experiment,
    and one that was not, the contact that did not accept will show after the
    assignment exclusion filter is applied.

    It does so by:

    1. creating an experiment
    2. creating a contact that has accepted an invitation to the experiment
    3. creating a contact that has NOT accepted an invitation to the experiment
    4. create a filter that for assignments that includes our experiment
    5. assert on the found contacts

    Fin. *)
let finds_unassigned_contacts =
  Test_utils.case
  @@ fun () ->
  (* 1. creating an experiment *)
  let& experiment = experiment () in
  (* 2. creating an session *)
  let& session = session ~experiment in
  (* 2. creating contacts *)
  let& assigned_contact = contact ~prefix:"invited" () in
  let& unassigned_contact = contact ~prefix:"probe" () in
  (* 3. send invitations *)
  let& () =
    invitation ~experiment ~contacts:[ assigned_contact; unassigned_contact ]
  in
  (* 4. only accept one of the invitations, creating the assignment *)
  let& () = assignment ~experiment ~contact:assigned_contact ~session in
  (* 5. create a filter that for assignments that includes our experiment *)
  let assignment_filter =
    let open Filter in
    let key : Key.t = Key.(Hardcoded Assignment) in
    let value =
      let exp_ids =
        [ Experiment.(experiment.id) ]
        |> CCList.map Experiment.Id.value
        |> CCList.map (fun value -> Filter.Str value)
      in
      Lst exp_ids
    in
    let operator = Operator.(List ListM.ContainsNone) in
    let predicate = Predicate.create key operator value in
    Filter.create None (Pred predicate)
  in
  let& found_contacts =
    Filter.find_filtered_contacts
      test_db
      Filter.MatchesFilter
      (Some assignment_filter)
  in
  (* FIXME(@leostera): since tests are not currently running in isolation, when
     we search for things we may find a lot more than we care about. This little
     filtering makes sure that we only ever return some of the users that we
     have created. This is a HACK and we shoudl fix it by ensuring every test is
     run in its own transaction. *)
  let found_contacts =
    CCList.filter
      (fun contact ->
         let open Contact in
         let open Pool_user in
         contact.user.id = unassigned_contact.user.id
         || contact.user.id = assigned_contact.user.id)
      found_contacts
  in
  (* 6. assert on the found contacts *)
  let& expected_contact =
    Contact.find test_db (Contact.id unassigned_contact)
  in
  Alcotest.(
    check
      int
      "wrong number of contacts returned"
      1
      (CCList.length found_contacts));
  let actual_contact = CCList.hd found_contacts in
  Alcotest.(
    check
      Test_utils.contact
      "wrong contact retrieved"
      expected_contact
      actual_contact);
  Lwt_result.lift (Ok ())
;;

(** This test verifies that given a contact that was invited to an experiment,
    that contact is properly excluded by the filter.

    It does so by:

    1. creating an experiment
    2. creating a contact that has accepted an invitation to the experiment
    3. create a filter that for assignments that includes our experiment
    4. assert on the found contacts

    Fin. *)
let filters_out_assigned_contacts =
  Test_utils.case
  @@ fun () ->
  (* 1. creating an experiment *)
  let& experiment = experiment () in
  (* 2. creating an session *)
  let& session = session ~experiment in
  (* 2. creating a contact that is invited to the experiment *)
  let& assigned_contact = contact ~prefix:"invited" () in
  let& () = invitation ~experiment ~contacts:[ assigned_contact ] in
  (* 4. only accept one of the invitations, creating the assignment *)
  let& () = assignment ~experiment ~contact:assigned_contact ~session in
  (* 3. create a filter that for assignments that includes our experiment *)
  let assignment_filter =
    let open Filter in
    let key : Key.t = Key.(Hardcoded Assignment) in
    let value =
      let exp_ids =
        [ Experiment.(experiment.id) ]
        |> CCList.map Experiment.Id.value
        |> CCList.map (fun value -> Filter.Str value)
      in
      Lst exp_ids
    in
    let operator = Operator.(List ListM.ContainsNone) in
    let predicate = Predicate.create key operator value in
    Filter.create None (Pred predicate)
  in
  let& found_contacts =
    Filter.find_filtered_contacts
      test_db
      Filter.MatchesFilter
      (Some assignment_filter)
  in
  (* FIXME(@leostera): since tests are not currently running in isolation, when
     we search for things we may find a lot more than we care about. This little
     filtering makes sure that we only ever return some of the users that we
     have created. This is a HACK and we shoudl fix it by ensuring every test is
     run in its own transaction. *)
  let found_contacts =
    CCList.filter
      (fun contact ->
         let open Contact in
         let open Pool_user in
         contact.user.id = assigned_contact.user.id)
      found_contacts
  in
  (* 4. assert on the found contacts *)
  Alcotest.(
    check
      int
      "wrong number of contacts returned"
      0
      (CCList.length found_contacts));
  Lwt_result.lift (Ok ())
;;
