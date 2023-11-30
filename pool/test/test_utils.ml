module Data = struct
  let database_label = "econ-test" |> Pool_database.Label.of_string
end

(* Testable *)
let event = Alcotest.testable Pool_event.pp Pool_event.equal

let partial_update =
  Alcotest.testable
    Custom_field.PartialUpdate.pp
    Custom_field.PartialUpdate.equal
;;

let language =
  Alcotest.testable Pool_common.Language.pp Pool_common.Language.equal
;;

let message_template =
  Alcotest.testable Message_template.pp Message_template.equal
;;

let tenant_smtp_auth = Alcotest.testable Email.SmtpAuth.pp Email.SmtpAuth.equal

let database_label =
  Alcotest.testable Pool_database.Label.pp Pool_database.Label.equal
;;

let error =
  Alcotest.testable Pool_common.Message.pp_error Pool_common.Message.equal_error
;;

let contact = Alcotest.testable Contact.pp Contact.equal
let smtp_auth = Alcotest.testable Email.SmtpAuth.pp Email.SmtpAuth.equal

let check_result ?(msg = "succeeds") =
  let open Alcotest in
  check (result (list event) error) msg
;;

let password = Alcotest.testable Pool_user.Password.pp Pool_user.Password.equal

let phone_nr =
  Alcotest.testable Pool_user.CellPhone.pp Pool_user.CellPhone.equal
;;

(* Helper functions *)

let setup_test () =
  let open Sihl.Configuration in
  let file_configuration = read_env_file () in
  let () = store @@ CCOption.value file_configuration ~default:[] in
  let () = Logs.set_level (Some Logs.Error) in
  let () = Logs.set_reporter Sihl.Log.default_reporter in
  Lwt.return_unit
;;

let get_or_failwith res =
  res
  |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
  |> CCResult.get_or_failwith
;;

let file_to_storage file =
  let open Database.SeedAssets in
  let stored_file =
    Sihl_storage.
      { id = file.Database.SeedAssets.id
      ; filename = file.filename
      ; filesize = file.filesize
      ; mime = file.mime
      }
  in
  let base64 = Base64.encode_exn file.body in
  let%lwt _ = Service.Storage.upload_base64 stored_file base64 in
  Lwt.return_unit
;;

let dummy_to_file (dummy : Database.SeedAssets.file) =
  let open Database.SeedAssets in
  let open Pool_common in
  let name = File.Name.create dummy.filename |> get_or_failwith in
  let filesize = File.Size.create dummy.filesize |> get_or_failwith in
  let mime_type = File.Mime.of_string dummy.mime |> get_or_failwith in
  File.
    { id = dummy.id |> Id.of_string
    ; name
    ; size = filesize
    ; mime_type
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

module Model = struct
  let create_sihl_user
    ?(id = Pool_common.Id.create ())
    ?(email =
      Format.asprintf "test+%s@econ.uzh.ch" (Uuidm.v `V4 |> Uuidm.to_string))
    ?(name = "Doe")
    ()
    =
    let surname = name in
    Sihl_user.
      { id = id |> Pool_common.Id.value
      ; email
      ; username = None
      ; name = Some surname
      ; given_name = Some "Jane"
      ; password = "somepassword" |> Sihl_user.Hashing.hash |> CCResult.get_exn
      ; status = Sihl_user.status_of_string "active" |> CCResult.get_exn
      ; admin = false
      ; confirmed = true
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_contact ?id ?name ?(with_terms_accepted = true) () =
    let sihl_user = create_sihl_user ?id ?name () in
    Contact.
      { user = sihl_user
      ; terms_accepted_at =
          (if with_terms_accepted
           then Pool_user.TermsAccepted.create_now () |> CCOption.return
           else None)
      ; language = Some Pool_common.Language.En
      ; experiment_type_preference = None
      ; cell_phone = Some ("+41791234567" |> Pool_user.CellPhone.of_string)
      ; paused = Pool_user.Paused.create false
      ; disabled = Pool_user.Disabled.create false
      ; verified = None
      ; email_verified =
          ()
          |> Ptime_clock.now
          |> Pool_user.EmailVerified.create
          |> CCOption.return
      ; num_invitations = NumberOfInvitations.init
      ; num_assignments = NumberOfAssignments.init
      ; num_show_ups = NumberOfShowUps.init
      ; num_no_shows = NumberOfNoShows.init
      ; num_participations = NumberOfParticipations.init
      ; firstname_version = Pool_common.Version.create ()
      ; lastname_version = Pool_common.Version.create ()
      ; paused_version = Pool_common.Version.create ()
      ; language_version = Pool_common.Version.create ()
      ; experiment_type_preference_version = Pool_common.Version.create ()
      ; import_pending = Pool_user.ImportPending.create false
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_admin ?id ?email () =
    () |> create_sihl_user ?id ?email |> Admin.create |> Pool_context.admin
  ;;

  let create_location ?(id = Pool_location.Id.create ()) () =
    Pool_location.
      { id
      ; name = Pool_location.Name.create "Online" |> get_or_failwith
      ; description = None
      ; link = None
      ; address = Pool_location.Address.Virtual
      ; status = Pool_location.Status.Active
      ; files = []
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_public_experiment () =
    Experiment.(
      Public.
        { id = Experiment.Id.create ()
        ; public_title = PublicTitle.create "public_title" |> get_or_failwith
        ; description =
            Description.create "A description for everyone"
            |> get_or_failwith
            |> CCOption.return
        ; direct_registration_disabled =
            false |> DirectRegistrationDisabled.create
        ; experiment_type = Some Pool_common.ExperimentType.Lab
        ; smtp_auth_id = None
        })
  ;;

  let create_experiment
    ?(id = Experiment.Id.create ())
    ?(title = "An Experiment")
    ?email_session_reminder_lead_time_hours
    ?filter
    ()
    =
    let show_error err = Pool_common.(Utils.error_to_string Language.En err) in
    let email_session_reminder_lead_time =
      let open CCResult in
      CCOption.bind email_session_reminder_lead_time_hours (fun h ->
        Ptime.Span.of_int_s @@ (h * 60 * 60)
        |> Pool_common.Reminder.LeadTime.create
        |> map_err show_error
        |> to_opt)
    in
    let open Experiment in
    let title = Title.create title |> get_or_failwith in
    let public_title = PublicTitle.create "public_title" |> get_or_failwith in
    let description =
      Description.create "A description for everyone" |> get_or_failwith
    in
    create
      ~id
      ~cost_center:("F-00000-11-22" |> CostCenter.of_string)
      ~description
      ~experiment_type:Pool_common.ExperimentType.Lab
      ?filter
      ?email_session_reminder_lead_time
      title
      public_title
      (DirectRegistrationDisabled.create false)
      (RegistrationDisabled.create false)
      (AllowUninvitedSignup.create false)
      (ExternalDataRequired.create false)
      (ShowExternalDataIdLinks.create false)
    |> get_or_failwith
  ;;

  let create_organisational_unit () =
    let open Organisational_unit in
    Name.create "SNS" |> get_or_failwith |> create
  ;;

  let experiment_to_public_experiment (experiment : Experiment.t) =
    Experiment.(
      Public.
        { id = experiment.id
        ; public_title = experiment.public_title
        ; description = experiment.description
        ; direct_registration_disabled = experiment.direct_registration_disabled
        ; experiment_type = experiment.experiment_type
        ; smtp_auth_id = None
        })
  ;;

  let create_waiting_list () =
    let contact = create_contact () in
    let experiment = create_experiment () in
    Waiting_list.
      { id = Pool_common.Id.create ()
      ; contact
      ; experiment
      ; admin_comment = None
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_waiting_list_from_experiment_and_contact experiment contact =
    Waiting_list.
      { id = Pool_common.Id.create ()
      ; contact
      ; experiment
      ; admin_comment = None
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_mailing ?id ?(limit = Mailing.Limit.default) () =
    let open Mailing in
    let start =
      Ptime.add_span
        (Ptime_clock.now ())
        Sihl.Time.(OneSecond |> duration_to_span)
      |> CCOption.get_exn_or "Time calculation failed!"
      |> StartAt.create
      |> get_or_failwith
    in
    let deadline =
      Ptime.add_span
        (Ptime_clock.now ())
        Sihl.Time.(OneHour |> duration_to_span)
      |> CCOption.get_exn_or "Time calculation failed!"
      |> EndAt.create
      |> get_or_failwith
    in
    create ?id Start.(StartAt start) deadline limit None |> get_or_failwith
  ;;

  let create_email
    ?(sender = "sender@mail.com")
    ?(recipient = "recipient@mail.com")
    ()
    =
    Sihl_email.create
      ~html:"<p>Hello</p>"
      ~cc:[ "cc1@mail.com"; "cc2@mail.com" ]
      ~bcc:[ "bcc@mail.com" ]
      ~sender
      ~recipient
      ~subject:"Subject"
      "Hello"
  ;;

  let create_text_message
    ?(sender = Pool_tenant.Title.of_string "UAST")
    cell_phone
    =
    Text_message.render_and_create cell_phone sender ("Hello world", [])
  ;;

  let hour = Ptime.Span.of_int_s @@ (60 * 60)

  let an_hour_ago () =
    let hour = Ptime.Span.of_int_s @@ (60 * 60) in
    Ptime.sub_span (Ptime_clock.now ()) hour
    |> CCOption.get_exn_or "Invalid start"
    |> Session.Start.create
  ;;

  let session_start_in timespan =
    timespan
    |> Ptime.add_span (Ptime_clock.now ())
    |> CCOption.get_exn_or "Invalid start"
    |> Session.Start.create
  ;;

  let in_an_hour () = Ptime.Span.of_int_s @@ (60 * 60) |> session_start_in
  let in_two_hours () = Ptime.Span.of_int_s @@ (60 * 60 * 2) |> session_start_in

  let create_session
    ?(id = Session.Id.create ())
    ?(location = create_location ())
    ?follow_up_to
    ?start
    ?email_reminder_sent_at
    ()
    =
    let open Session in
    create
      ~id
      ?follow_up_to
      (start |> CCOption.value ~default:(in_an_hour ()))
      (Duration.create hour |> get_or_failwith)
      location
      (ParticipantAmount.create 30 |> get_or_failwith)
      (ParticipantAmount.create 1 |> get_or_failwith)
      (ParticipantAmount.create 4 |> get_or_failwith)
    |> fun session -> { session with email_reminder_sent_at }
  ;;

  let create_public_session ?start () =
    let Session.
          { id
          ; follow_up_to
          ; start
          ; duration
          ; description
          ; location
          ; max_participants
          ; min_participants
          ; overbook
          ; assignment_count
          ; canceled_at
          ; _
          }
      =
      create_session ?start ()
    in
    Session.Public.
      { id
      ; follow_up_to
      ; start
      ; duration
      ; description
      ; location
      ; max_participants
      ; min_participants
      ; overbook
      ; assignment_count
      ; canceled_at
      }
  ;;

  let session_to_public_session
    { Session.id
    ; follow_up_to
    ; start
    ; duration
    ; description
    ; location
    ; max_participants
    ; min_participants
    ; overbook
    ; assignment_count
    ; canceled_at
    ; _
    }
    =
    Session.Public.
      { id
      ; follow_up_to
      ; start
      ; duration
      ; description
      ; location
      ; max_participants
      ; min_participants
      ; overbook
      ; assignment_count
      ; canceled_at
      }
  ;;

  let session_to_session_base
    ({ Session.start
     ; duration
     ; description
     ; limitations
     ; max_participants
     ; min_participants
     ; overbook
     ; email_reminder_lead_time
     ; text_message_reminder_lead_time
     ; _
     } :
      Session.t)
    : Session.base
    =
    Session.
      { start
      ; duration
      ; description
      ; limitations
      ; max_participants
      ; min_participants
      ; overbook
      ; email_reminder_lead_time
      ; text_message_reminder_lead_time
      }
  ;;

  let fully_book_session session =
    Session.
      { session with
        max_participants = ParticipantAmount.create 5 |> get_or_failwith
      ; min_participants = ParticipantAmount.create 0 |> get_or_failwith
      ; overbook = ParticipantAmount.create 0 |> get_or_failwith
      ; assignment_count = 5 |> AssignmentCount.create |> get_or_failwith
      }
  ;;

  let fully_book_public_session session =
    Session.Public.
      { session with
        max_participants = Session.ParticipantAmount.create 5 |> get_or_failwith
      ; min_participants = Session.ParticipantAmount.create 0 |> get_or_failwith
      ; overbook = Session.ParticipantAmount.create 0 |> get_or_failwith
      ; assignment_count =
          5 |> Session.AssignmentCount.create |> get_or_failwith
      }
  ;;

  let create_follow_up_session session main =
    Session.{ session with follow_up_to = Some main.id }
  ;;

  let create_assignment
    ?no_show
    ?participated
    ?external_data_id
    ?(contact = create_contact ())
    ()
    =
    let open CCOption in
    let open Assignment in
    let no_show = no_show >|= NoShow.create in
    let participated = participated >|= Participated.create in
    let external_data_id = external_data_id >|= ExternalDataId.of_string in
    { id = Id.create ()
    ; contact
    ; no_show
    ; participated
    ; matches_filter = MatchesFilter.init
    ; canceled_at = None
    ; marked_as_deleted = MarkedAsDeleted.init
    ; external_data_id
    ; reminder_manually_last_sent_at = None
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
  ;;

  let create_message_template ?label ?language ?entity_uuid () =
    let open Message_template in
    let exn = CCResult.get_exn in
    let label = CCOption.value ~default:Label.AssignmentConfirmation label in
    let language = CCOption.value ~default:Pool_common.Language.En language in
    { id = Id.create ()
    ; label
    ; language
    ; entity_uuid
    ; email_subject = "Subject" |> EmailSubject.create |> exn
    ; email_text = "<div>Hello</div>" |> EmailText.create |> exn
    ; plain_text = "Hello" |> PlainText.create |> exn
    ; sms_text = "Hello" |> SmsText.create |> exn
    }
  ;;
end

module FilterHelper = struct
  let equal = Filter.Operator.(Equality.Equal |> equality)
end

module Repo = struct
  let first_contact () =
    let open CCFun.Infix in
    let open Utils.Lwt_result.Infix in
    Contact.find_all Data.database_label () ||> fst %> CCList.hd
  ;;

  let first_tag () =
    let open Utils.Lwt_result.Infix in
    Tags.find_all Data.database_label ||> CCList.hd
  ;;

  let all_experiments () =
    let open Utils.Lwt_result.Infix in
    Experiment.find_all Data.database_label ||> fst
  ;;

  let first_experiment () =
    let open Utils.Lwt_result.Infix in
    all_experiments () ||> CCList.hd
  ;;

  let create_experiment ?(id = Experiment.Id.create ()) ?filter () =
    let experiment = Model.create_experiment ~id ?filter () in
    let%lwt () =
      Experiment.Created experiment
      |> Pool_event.experiment
      |> Pool_event.handle_event Data.database_label
    in
    Lwt.return experiment
  ;;

  let first_location () =
    let open Utils.Lwt_result.Infix in
    Pool_location.find_all Data.database_label ||> CCList.hd
  ;;
end

let case test_fn (_switch : Lwt_switch.t) () : unit Lwt.t =
  Sihl.Database.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
    Conn.with_transaction (fun () ->
      test_fn () |> Lwt_result.map_error (fun err -> `Test_error err)))
  |> Lwt.map (fun result ->
    match result with
    | Error (`Test_error err) -> Pool_common.Utils.get_or_failwith (Error err)
    | Error ((#Caqti_error.load_or_connect | #Caqti_error.transact) as err) ->
      raise (Caqti_error.Exn err)
    | Ok () -> ())
;;
