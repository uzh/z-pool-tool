(* test 1: 1 invited contact, 1 probe contact test 2: 1 invited contact, no
   contacts left *)

let ( let@ ) = Result.bind
let ( let* ) x f = Lwt_result.bind (Lwt_result.lift x) f
let ( let& ) = Lwt_result.bind
let test_db = Test_utils.Data.database_label

(** This test verifies that given a contact that was invited to an experiment,
    and one that was not, the contact that was not invited will show after the
    invitation exclusion filter is applied.

    It does so by:

    1. creating a session
    2. creating an experiment
    3. creating a contact that is invited to the experiment
    4. creating a contact that is NOT invited to the experiment
    5. create all entities in the database
    6. create a filter that for invitations that includes our experiment
    7. assert on the found contacts

    Fin. *)
let test () =
  (* 1. create a session *)
  let _session =
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
  in
  (* 2. create an experiment *)
  let& experiment =
    let open Experiment in
    let experiment_id = Id.create () in
    let* title = Title.create "an-experiment-title" in
    let* public_title = PublicTitle.create "a-public-title" in
    let direct_registration_disabled =
      DirectRegistrationDisabled.create false
    in
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
  in
  (* 3. create a contact that is invited to the experiment *)
  let invited_contact_id = Contact.Id.create () in
  let& invited_contact =
    let open Contact in
    let* email = Pool_user.EmailAddress.create "user@domain.test" in
    let* password = Pool_user.Password.create_unvalidated "a-password" in
    let* firstname = Pool_user.Firstname.create "firstname" in
    let* lastname = Pool_user.Lastname.create "lastname" in
    let terms_accepted_at =
      Pool_user.TermsAccepted.create (Ptime_clock.now ()) |> Option.some
    in
    let language = Pool_common.Language.En |> Option.some in
    let contact_created =
      Contact.created
        { user_id = invited_contact_id
        ; email
        ; password
        ; firstname
        ; lastname
        ; terms_accepted_at
        ; language
        }
      |> Pool_event.contact
    in
    let& () =
      Pool_event.handle_event test_db contact_created |> Lwt_result.ok
    in
    Contact.find test_db invited_contact_id
  in
  let invitation =
    Invitation.{ experiment; mailing = None; contacts = [ invited_contact ] }
  in
  (* 4. create a contact that is NOT invited to the experiment *)
  let probe_contact_id = Contact.Id.create () in
  let& probe_contact =
    let open Contact in
    let* email = Pool_user.EmailAddress.create "probe@domain.test" in
    let* password = Pool_user.Password.create_unvalidated "a-password" in
    let* firstname = Pool_user.Firstname.create "probe-firstname" in
    let* lastname = Pool_user.Lastname.create "probe-lastname" in
    let terms_accepted_at =
      Pool_user.TermsAccepted.create (Ptime_clock.now ()) |> Option.some
    in
    let language = Pool_common.Language.En |> Option.some in
    let contact =
      { user_id = probe_contact_id
      ; email
      ; password
      ; firstname
      ; lastname
      ; terms_accepted_at
      ; language
      }
    in
    Lwt_result.lift (Ok contact)
  in
  let events =
    [ Contact.created probe_contact |> Pool_event.contact
    ; Invitation.Created invitation |> Pool_event.invitation
    ]
  in
  (* 5. create all entities in the database *)
  let& () = Pool_event.handle_events test_db events |> Lwt_result.ok in
  (* 6. create a filter that for invitations that includes our experiment *)
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
    let operator = Operator.(ListM.ContainsSome |> Operator.list) in
    let predicate = Predicate.create key operator value in
    Filter.create None (Pred predicate)
  in
  let& found_contacts =
    Filter.find_filtered_contacts
      test_db
      Filter.MatchesFilter
      (Some invitation_filter)
  in
  (* 7. assert on the found contacts *)
  Alcotest.(
    check
      int
      "wrong number of contacts returned"
      2112
      (List.length found_contacts));
  Lwt_result.lift (Ok ())
;;

let () = Test_utils.run_test test
