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

let tenant_smtp_auth =
  Alcotest.testable Pool_tenant.SmtpAuth.pp Pool_tenant.SmtpAuth.equal
;;

let error =
  Alcotest.testable Pool_common.Message.pp_error Pool_common.Message.equal_error
;;

let contact = Alcotest.testable Contact.pp Contact.equal

let check_result ?(msg = "succeeds") =
  let open Alcotest in
  check (result (list event) error) msg
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

let get_or_failwith_pool_error res =
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
  let name = File.Name.create dummy.filename |> get_or_failwith_pool_error in
  let filesize =
    File.Size.create dummy.filesize |> get_or_failwith_pool_error
  in
  let mime_type =
    File.Mime.of_string dummy.mime |> get_or_failwith_pool_error
  in
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
  let create_sihl_user () =
    Sihl_user.
      { id = Pool_common.Id.(create () |> value)
      ; email =
          Format.asprintf "test+%s@econ.uzh.ch" (Uuidm.v `V4 |> Uuidm.to_string)
      ; username = None
      ; name = Some "Doe"
      ; given_name = Some "Jane"
      ; password =
          "somepassword" |> Sihl_user.Hashing.hash |> CCResult.get_or_failwith
      ; status = Sihl_user.status_of_string "active" |> CCResult.get_or_failwith
      ; admin = false
      ; confirmed = true
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_contact ?(with_terms_accepted = true) () =
    Contact.
      { user = create_sihl_user ()
      ; terms_accepted_at =
          (if with_terms_accepted
           then Pool_user.TermsAccepted.create_now () |> CCOption.pure
           else None)
      ; language = Some Pool_common.Language.En
      ; experiment_type_preference = None
      ; paused = Pool_user.Paused.create false
      ; disabled = Pool_user.Disabled.create false
      ; verified = None
      ; email_verified =
          ()
          |> Ptime_clock.now
          |> Pool_user.EmailVerified.create
          |> CCOption.pure
      ; num_invitations = NumberOfInvitations.init
      ; num_assignments = NumberOfAssignments.init
      ; num_show_ups = NumberOfShowUps.init
      ; num_participations = NumberOfParticipations.init
      ; firstname_version = Pool_common.Version.create ()
      ; lastname_version = Pool_common.Version.create ()
      ; paused_version = Pool_common.Version.create ()
      ; language_version = Pool_common.Version.create ()
      ; experiment_type_preference_version = Pool_common.Version.create ()
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_admin () =
    () |> create_sihl_user |> Admin.create |> Pool_context.admin
  ;;

  let create_location () =
    Pool_location.
      { id = Pool_location.Id.create ()
      ; name =
          Pool_location.Name.create "Online"
          |> Pool_common.Utils.get_or_failwith
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
    let show_error err = Pool_common.(Utils.error_to_string Language.En err) in
    Experiment.(
      Public.
        { id = Experiment.Id.create ()
        ; public_title =
            PublicTitle.create "public_title"
            |> CCResult.map_err show_error
            |> CCResult.get_or_failwith
        ; description =
            Description.create "A description for everyone"
            |> CCResult.map_err show_error
            |> CCResult.get_or_failwith
        ; direct_registration_disabled =
            false |> DirectRegistrationDisabled.create
        ; experiment_type = Some Pool_common.ExperimentType.Lab
        })
  ;;

  let create_experiment () =
    let show_error err = Pool_common.(Utils.error_to_string Language.En err) in
    Experiment.
      { id = Experiment.Id.create ()
      ; title =
          Title.create "An Experiment"
          |> CCResult.map_err show_error
          |> CCResult.get_or_failwith
      ; public_title =
          PublicTitle.create "public_title"
          |> CCResult.map_err show_error
          |> CCResult.get_or_failwith
      ; description =
          Description.create "A description for everyone"
          |> CCResult.map_err show_error
          |> CCResult.get_or_failwith
      ; filter = None
      ; session_reminder_lead_time =
          Ptime.Span.of_int_s @@ (60 * 60)
          |> Pool_common.Reminder.LeadTime.create
          |> CCResult.map_err show_error
          |> CCResult.to_opt
      ; direct_registration_disabled =
          false |> DirectRegistrationDisabled.create
      ; registration_disabled = false |> RegistrationDisabled.create
      ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
      ; experiment_type = Some Pool_common.ExperimentType.Lab
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
  ;;

  let experiment_to_public_experiment (experiment : Experiment.t) =
    Experiment.(
      Public.
        { id = experiment.id
        ; public_title = experiment.public_title
        ; description = experiment.description
        ; direct_registration_disabled = experiment.direct_registration_disabled
        ; experiment_type = experiment.experiment_type
        })
  ;;

  let create_waiting_list () =
    let contact = create_contact () in
    let experiment = create_experiment () in
    Waiting_list.
      { id = Pool_common.Id.create ()
      ; contact
      ; experiment
      ; comment = None
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_waiting_list_from_experiment_and_contact experiment contact =
    Waiting_list.
      { id = Pool_common.Id.create ()
      ; contact
      ; experiment
      ; comment = None
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_mailing ?id ?(rate = Mailing.Rate.default) () =
    let open Mailing in
    let start =
      Ptime.add_span
        (Ptime_clock.now ())
        Sihl.Time.(OneSecond |> duration_to_span)
      |> CCOption.get_exn_or "Time calculation failed!"
      |> StartAt.create
      |> get_or_failwith_pool_error
    in
    let deadline =
      Ptime.add_span
        (Ptime_clock.now ())
        Sihl.Time.(OneHour |> duration_to_span)
      |> CCOption.get_exn_or "Time calculation failed!"
      |> EndAt.create
      |> get_or_failwith_pool_error
    in
    create ?id Start.(StartAt start) deadline rate None
    |> get_or_failwith_pool_error
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

  let hour = Ptime.Span.of_int_s @@ (60 * 60)

  let an_hour_ago () =
    let hour = Ptime.Span.of_int_s @@ (60 * 60) in
    Ptime.sub_span (Ptime_clock.now ()) hour
    |> CCOption.get_exn_or "Invalid start"
    |> Session.Start.create
  ;;

  let in_an_hour () =
    let hour = Ptime.Span.of_int_s @@ (60 * 60) in
    Ptime.add_span (Ptime_clock.now ()) hour
    |> CCOption.get_exn_or "Invalid start"
    |> Session.Start.create
  ;;

  let create_session ?(id = Pool_common.Id.create ()) ?follow_up_to ?start () =
    let open Session in
    let start = start |> CCOption.value ~default:(in_an_hour ()) in
    { id
    ; follow_up_to
    ; has_follow_ups = false
    ; start
    ; duration = Duration.create hour |> get_or_failwith_pool_error
    ; description = None
    ; location = create_location ()
    ; max_participants =
        ParticipantAmount.create 30 |> get_or_failwith_pool_error
    ; min_participants =
        ParticipantAmount.create 1 |> get_or_failwith_pool_error
    ; overbook = ParticipantAmount.create 4 |> get_or_failwith_pool_error
    ; reminder_lead_time = None
    ; reminder_sent_at = None
    ; assignment_count =
        0 |> AssignmentCount.create |> get_or_failwith_pool_error
    ; show_up_count = 0 |> ShowUpCount.create |> get_or_failwith_pool_error
    ; participant_count =
        0 |> ParticipantCount.create |> get_or_failwith_pool_error
    ; closed_at = None
    ; canceled_at = None
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
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

  let fully_book_session session =
    let get_or_failwith = Pool_common.Utils.get_or_failwith in
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
        max_participants =
          Session.ParticipantAmount.create 5
          |> Pool_common.Utils.get_or_failwith
      ; min_participants =
          Session.ParticipantAmount.create 0
          |> Pool_common.Utils.get_or_failwith
      ; overbook =
          Session.ParticipantAmount.create 0
          |> Pool_common.Utils.get_or_failwith
      ; assignment_count =
          5
          |> Session.AssignmentCount.create
          |> Pool_common.Utils.get_or_failwith
      }
  ;;

  let create_follow_up_session session main =
    Session.{ session with follow_up_to = Some main.id }
  ;;

  let create_assignment () =
    Assignment.
      { id = Pool_common.Id.create ()
      ; contact = create_contact ()
      ; show_up = None
      ; participated = None
      ; matches_filter = MatchesFilter.init
      ; canceled_at = None
      ; marked_as_deleted = MarkedAsDeleted.init
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

module Repo = struct
  let create_contact pool () =
    let open Utils.Lwt_result.Infix in
    let contact = Model.create_contact ~with_terms_accepted:false () in
    let verified =
      if contact.Contact.user.Sihl_user.confirmed
      then Contact.[ Verified contact ]
      else []
    in
    let%lwt () =
      [ Contact.(
          Created
            { user_id = Contact.id contact
            ; email = Contact.email_address contact
            ; password =
                contact.Contact.user.Sihl_user.password
                |> Pool_user.Password.create
                |> get_or_failwith_pool_error
            ; firstname = Contact.firstname contact
            ; lastname = Contact.lastname contact
            ; terms_accepted_at = None
            ; language = contact.language
            })
      ]
      @ verified
      |> Lwt_list.iter_s (Contact.handle_event pool)
    in
    contact |> Contact.id |> Contact.find pool ||> get_or_failwith_pool_error
  ;;
end
