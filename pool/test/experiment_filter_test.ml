let ( let@ ) = Result.bind
let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f
let ( let& ) = Lwt_result.bind
let test_db = Test_utils.Data.database_label

let session () =
  let open Session in
  let sid = Id.create () in
  let now = Ptime_clock.now () in
  let session_start = Start.create now in
  let* session_duration =
    Duration.create (Ptime.diff now (Ptime_clock.now ()))
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
    Pool_event.handle_event test_db experiment_created |> Lwt_result.ok
  in
  Experiment.find test_db experiment_id
;;

let contact ~prefix () =
  let open Contact in
  let invited_contact_id = Id.create () in
  let* email =
    let email =
      Format.asprintf "%s+%s@domain.test" prefix (Id.value invited_contact_id)
    in
    Pool_user.EmailAddress.create email
  in
  let* password = Pool_user.Password.create_unvalidated "a-password" in
  let* firstname = Pool_user.Firstname.create "firstname" in
  let* lastname = Pool_user.Lastname.create "lastname" in
  let terms_accepted_at =
    Pool_user.TermsAccepted.create (Ptime_clock.now ()) |> Option.some
  in
  let language = Pool_common.Language.En |> Option.some in
  let contact_created =
    [ Contact.created
        { user_id = invited_contact_id
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
  let& () = Pool_event.handle_events test_db contact_created |> Lwt_result.ok in
  let& contact = Contact.find test_db invited_contact_id in
  let%lwt token = Email.create_token test_db email in
  let* verification_events =
    let open Cqrs_command.User_command in
    let created_email =
      Email.Created (email, token, invited_contact_id)
      |> Pool_event.email_verification
    in
    let email = Email.create email contact.user token in
    let@ verify_events = VerifyEmail.handle (Contact contact) email in
    Ok (created_email :: verify_events)
  in
  let& () =
    Pool_event.handle_events test_db verification_events |> Lwt_result.ok
  in
  let& contact = Contact.find test_db invited_contact_id in
  let verification_events =
    [ Contact.Verified contact |> Pool_event.contact ]
  in
  let& () =
    Pool_event.handle_events test_db verification_events |> Lwt_result.ok
  in
  let& contact = Contact.find test_db invited_contact_id in
  Lwt_result.lift (Ok contact)
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
            (fun _ ->
              Sihl_email.create
                ~sender:"sender"
                ~recipient:"recipient"
                ~subject:"subject"
                "body"
              |> Result.ok)
        ; mailing = None
        })
  in
  let& () = Pool_event.handle_events test_db events |> Lwt_result.ok in
  Lwt_result.lift (Ok ())
;;

(** This test verifies that given a contact that was invited to an experiment,
    and one that was not, the contact that was not invited will show after the
    invitation exclusion filter is applied.

    It does so by:

    1. creating a session
    2. creating an experiment
    3. creating a contact that is invited to the experiment
    4. creating a contact that is NOT invited to the experiment
    5. create a filter that for invitations that includes our experiment
    6. assert on the found contacts

    Fin. *)
let test =
  Test_utils.case
  @@ fun () ->
  (* 1. creating a session *)
  (* let _session = session () in *)
  (* 2. creating an experiment *)
  let& experiment = experiment () in
  (* 3. creating a contact that is invited to the experiment *)
  let& invited_contact = contact ~prefix:"invited" () in
  let& _invitation = invitation ~experiment ~contacts:[ invited_contact ] in
  (* 4. creating a contact that is NOT invited to the experiment *)
  let& expected_contact = contact ~prefix:"probe" () in
  (* 5. create a filter that for invitations that includes our experiment *)
  let invitation_filter =
    let open Filter in
    let key : Key.t = Key.(Hardcoded Invitation) in
    let value =
      let exp_ids =
        [ Experiment.(experiment.id) ]
        |> List.map Experiment.Id.value
        |> List.map Filter.Value.string
      in
      Lst exp_ids
    in
    let operator = Operator.(List ListM.ContainsSome) in
    let predicate = Predicate.create key operator value in
    Filter.create None (Not (Pred predicate))
  in
  let& found_contacts =
    Filter.find_filtered_contacts
      test_db
      Filter.MatchesFilter
      (Some invitation_filter)
  in
  let found_contacts =
    List.filter
      (fun contact ->
        let open Contact in
        let open Sihl_user in
        contact.user.id = invited_contact.user.id
        || contact.user.id = expected_contact.user.id)
      found_contacts
  in
  (* 6. assert on the found contacts *)
  Alcotest.(
    check int "wrong number of contacts returned" 1 (List.length found_contacts));
  let actual_contact = List.hd found_contacts in
  Alcotest.(
    check
      Test_utils.contact
      "wrong contact retrieved"
      expected_contact
      actual_contact);
  Lwt_result.lift (Ok ())
;;
