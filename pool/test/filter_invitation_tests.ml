let ( let@ ) = Result.bind
let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f
let ( let& ) = Lwt_result.bind
let test_db = Test_utils.Data.database_label

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
            (fun (_ : Contact.t) ->
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

    1. creating an experiment
    2. creating a contact that is invited to the experiment
    3. creating a contact that is NOT invited to the experiment
    4. create a filter that for invitations that includes our experiment
    5. assert on the found contacts

    Fin. *)
let finds_uninvited_contacts =
  Test_utils.case
  @@ fun () ->
  (* 1. creating an experiment *)
  let& experiment = experiment () in
  (* 2. creating a contact that is invited to the experiment *)
  let& invited_contact = contact ~prefix:"invited" () in
  let& () = invitation ~experiment ~contacts:[ invited_contact ] in
  (* 3. creating a contact that is NOT invited to the experiment *)
  let& expected_contact = contact ~prefix:"probe" () in
  (* 4. create a filter that for invitations that includes our experiment *)
  let invitation_filter =
    let open Filter in
    let key : Key.t = Key.(Hardcoded Invitation) in
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
      (Some invitation_filter)
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
        let open Sihl_user in
        contact.user.id = invited_contact.user.id
        || contact.user.id = expected_contact.user.id)
      found_contacts
  in
  (* 5. assert on the found contacts *)
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
    2. creating a contact that is invited to the experiment
    3. create a filter that for invitations that includes our experiment
    4. assert on the found contacts

    Fin. *)
let filters_out_invited_contacts =
  Test_utils.case
  @@ fun () ->
  (* 1. creating an experiment *)
  let& experiment = experiment () in
  (* 2. creating a contact that is invited to the experiment *)
  let& invited_contact = contact ~prefix:"invited" () in
  let& () = invitation ~experiment ~contacts:[ invited_contact ] in
  (* 3. create a filter that for invitations that includes our experiment *)
  let invitation_filter =
    let open Filter in
    let key : Key.t = Key.(Hardcoded Invitation) in
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
      (Some invitation_filter)
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
        let open Sihl_user in
        contact.user.id = invited_contact.user.id)
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
