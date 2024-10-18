module Data = struct
  let database_label = "econ-test" |> Database.Label.of_string
end

(* Testable *)
let annoncement = Announcement.(Alcotest.testable pp equal)
let contact = Contact.(Alcotest.testable pp equal)
let database_label = Database.Label.(Alcotest.testable pp equal)
let error = Pool_message.Error.(Alcotest.testable pp equal)
let event = Pool_event.(Alcotest.testable pp equal)
let filter = Filter.(Alcotest.testable pp equal)
let language = Pool_common.Language.(Alcotest.testable pp equal)
let message_template = Message_template.(Alcotest.testable pp equal)
let partial_update = Custom_field.PartialUpdate.(Alcotest.testable pp equal)
let password = Pool_user.Password.(Alcotest.testable pp equal)
let password_plain = Pool_user.Password.Plain.(Alcotest.testable pp equal)
let phone_nr = Pool_user.CellPhone.(Alcotest.testable pp equal)
let smtp_auth = Email.SmtpAuth.(Alcotest.testable pp equal)
let time_window_testable = Time_window.(Alcotest.testable pp equal)

let check_result ?(msg = "succeeds") =
  let open Alcotest in
  check (result (list event) error) msg
;;

(* Helper functions *)

let setup_test
  ?(log_level = Logs.Info)
  ?(reporter = Logger.lwt_file_reporter ())
  ()
  =
  let open Sihl.Configuration in
  let () = read_env_file () |> CCOption.value ~default:[] |> store in
  let () = Logs.set_level (Some log_level) in
  let () = Logs.set_reporter reporter in
  Lwt.return_unit
;;

let get_or_failwith res =
  res
  |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
  |> CCResult.get_or_failwith
;;

let sort_events =
  CCList.stable_sort Pool_event.(fun a b -> CCString.compare (show a) (show b))
;;

let urlencoded_update urlencoded updates =
  let open CCOption in
  CCList.map
    (fun (k, v) ->
      CCList.find_opt (fun (check, _) -> check k) updates
      >|= snd
      >|= CCList.return
      |> value ~default:v
      |> CCPair.make k)
    urlencoded
;;

let urlencoded_remove urlencoded validation =
  CCList.filter CCFun.(fst %> validation %> not) urlencoded
;;

let file_to_storage file =
  let open Seed.Assets in
  let stored_file =
    Sihl_storage.
      { id = file.Seed.Assets.id
      ; filename = file.filename
      ; filesize = file.filesize
      ; mime = file.mime
      }
  in
  let base64 = Base64.encode_exn file.body in
  let%lwt _ = Storage.upload_base64 Data.database_label stored_file base64 in
  Lwt.return_unit
;;

let dummy_to_file (dummy : Seed.Assets.file) =
  let open Seed.Assets in
  let open Pool_common in
  let name = File.Name.create dummy.filename |> get_or_failwith in
  let filesize = File.Size.create dummy.filesize |> get_or_failwith in
  let mime_type = File.Mime.of_string dummy.mime |> get_or_failwith in
  File.
    { id = dummy.id |> Id.of_string
    ; name
    ; size = filesize
    ; mime_type
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

module Model = struct
  let create_announcement
    ?id
    ?start_at
    ?end_at
    ?(show_to_admins = true)
    ?(show_to_contacts = true)
    ()
    =
    let open Announcement in
    let text =
      Text.create [ Pool_common.Language.En, "text" ] |> get_or_failwith
    in
    create
      ?id
      text
      start_at
      end_at
      (ShowToAdmins.create show_to_admins)
      (ShowToContacts.create show_to_contacts)
  ;;

  let password =
    Pool_user.Password.Plain.(
      create "Somepassword1!" |> validate |> get_or_failwith)
  ;;

  let create_user
    ?(id = Pool_user.Id.create ())
    ?(email =
      Format.asprintf "test+%s@econ.uzh.ch" (Uuidm.v `V4 |> Uuidm.to_string)
      |> Pool_user.EmailAddress.of_string)
    ?(lastname = Pool_user.Lastname.of_string "Doe")
    ()
    =
    { Pool_user.id
    ; email
    ; lastname
    ; firstname = Pool_user.Firstname.of_string "Jane"
    ; status = Pool_user.Status.Active
    ; admin = Pool_user.IsAdmin.create false
    ; confirmed = Pool_user.Confirmed.create true
    }
  ;;

  let create_contact ?id ?language ?lastname ?(with_terms_accepted = true) () =
    let user =
      create_user ?id:(CCOption.map Contact.Id.to_user id) ?lastname ()
    in
    { Contact.user
    ; terms_accepted_at =
        (if with_terms_accepted
         then Pool_user.TermsAccepted.create_now () |> CCOption.return
         else None)
    ; language
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
    ; num_invitations = Contact.NumberOfInvitations.init
    ; num_assignments = Contact.NumberOfAssignments.init
    ; num_show_ups = Contact.NumberOfShowUps.init
    ; num_no_shows = Contact.NumberOfNoShows.init
    ; num_participations = Contact.NumberOfParticipations.init
    ; firstname_version = Pool_common.Version.create ()
    ; lastname_version = Pool_common.Version.create ()
    ; paused_version = Pool_common.Version.create ()
    ; language_version = Pool_common.Version.create ()
    ; experiment_type_preference_version = Pool_common.Version.create ()
    ; import_pending = Pool_user.ImportPending.create false
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
  ;;

  let create_admin ?id ?email () =
    ()
    |> create_user ?id ?email
    |> Admin.create
         ~email_verified:(Some (Pool_user.EmailVerified.create_now ()))
    |> Pool_context.admin
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
      ; created_at = Pool_common.CreatedAt.create_now ()
      ; updated_at = Pool_common.UpdatedAt.create_now ()
      }
  ;;

  let create_public_experiment () =
    let open Experiment in
    Public.create
      ~description:
        (PublicDescription.create "A description for everyone"
         |> get_or_failwith)
      ~experiment_type:Pool_common.ExperimentType.Lab
      (Id.create ())
      (PublicTitle.create "public_title" |> get_or_failwith)
      (DirectRegistrationDisabled.create false)
  ;;

  let create_experiment
    ?(id = Experiment.Id.create ())
    ?(title = "An Experiment")
    ?email_session_reminder_lead_time_hours
    ?filter
    ?online_experiment
    ()
    =
    let show_error err = Pool_common.(Utils.error_to_string Language.En err) in
    let email_session_reminder_lead_time =
      let open CCResult in
      CCOption.bind email_session_reminder_lead_time_hours (fun h ->
        Ptime.Span.of_int_s @@ (h * 60 * 60)
        |> Pool_common.Reminder.EmailLeadTime.create
        |> map_err show_error
        |> to_opt)
    in
    let title = Experiment.Title.create title |> get_or_failwith in
    let public_title =
      Experiment.PublicTitle.create "public_title" |> get_or_failwith
    in
    let internal_description =
      Experiment.InternalDescription.create "A description for everyone"
      |> get_or_failwith
    in
    Experiment.create
      ~id
      ~cost_center:("F-00000-11-22" |> Experiment.CostCenter.of_string)
      ~internal_description
      ~experiment_type:Pool_common.ExperimentType.Lab
      ?filter
      ?email_session_reminder_lead_time
      ?online_experiment
      title
      public_title
      (Experiment.DirectRegistrationDisabled.create false)
      (Experiment.RegistrationDisabled.create false)
      (Experiment.AllowUninvitedSignup.create false)
      (Experiment.ExternalDataRequired.create false)
      (Experiment.ShowExternalDataIdLinks.create false)
    |> get_or_failwith
  ;;

  let create_organisational_unit () =
    let open Organisational_unit in
    Name.create "SNS" |> get_or_failwith |> create
  ;;

  let create_waiting_list () =
    let contact = create_contact () in
    let experiment = create_experiment () in
    { Waiting_list.id = Waiting_list.Id.create ()
    ; contact
    ; experiment
    ; admin_comment = None
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
  ;;

  let create_waiting_list_from_experiment_and_contact experiment contact =
    { Waiting_list.id = Waiting_list.Id.create ()
    ; contact
    ; experiment
    ; admin_comment = None
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
  ;;

  let create_mailing ?id ?start ?(limit = Mailing.Limit.default) () =
    let open Mailing in
    let start =
      let default () =
        let start_at =
          Ptime.add_span
            (Ptime_clock.now ())
            Sihl.Time.(OneSecond |> duration_to_span)
          |> CCOption.get_exn_or "Time calculation failed!"
          |> StartAt.create
          |> get_or_failwith
        in
        Start.StartAt start_at
      in
      CCOption.value ~default:(default ()) start
    in
    let deadline =
      Ptime.add_span
        (Ptime_clock.now ())
        Sihl.Time.(OneHour |> duration_to_span)
      |> CCOption.get_exn_or "Time calculation failed!"
      |> EndAt.create
      |> get_or_failwith
    in
    create ?id start deadline limit None |> get_or_failwith
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

  let create_email_job
    ?job_ctx
    ?message_template
    ?email_address
    ?smtp_auth_id
    ()
    =
    create_email ?recipient:email_address ()
    |> Email.Service.Job.create ?smtp_auth_id
    |> Email.create_dispatch ?job_ctx ?message_template
  ;;

  let create_text_message
    ?(sender = Pool_tenant.GtxSender.of_string "UAST")
    cell_phone
    =
    Text_message.render_and_create cell_phone sender ("Hello world", [])
  ;;

  let create_text_message_job ?sender ?message_template ?job_ctx cell_phone =
    create_text_message ?sender cell_phone
    |> Text_message.create_job ?message_template ?job_ctx
  ;;

  let hour = Ptime.Span.of_int_s @@ (60 * 60)
  let two_hours = Ptime.Span.of_int_s @@ (60 * 60 * 2)

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
    ?(duration = Session.Duration.create hour |> get_or_failwith)
    ?follow_up_to
    ?start
    ?email_reminder_sent_at
    ?(experiment = create_experiment ())
    ()
    =
    let open Session in
    create
      ~id
      ?follow_up_to
      (start |> CCOption.value ~default:(in_an_hour ()))
      duration
      location
      (ParticipantAmount.create 30 |> get_or_failwith)
      (ParticipantAmount.create 1 |> get_or_failwith)
      (ParticipantAmount.create 4 |> get_or_failwith)
      experiment
    |> fun session -> { session with email_reminder_sent_at }
  ;;

  let create_timewindow
    ?(id = Session.Id.create ())
    ?(duration = Session.Duration.create hour |> get_or_failwith)
    ?start
    ?(experiment = create_experiment ())
    ()
    =
    Time_window.create
      ~id
      (start |> CCOption.value ~default:(in_an_hour ()))
      duration
      experiment
  ;;

  let create_public_session ?start () =
    let Session.
          { id
          ; follow_up_to
          ; start
          ; duration
          ; public_description
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
      ; description = public_description
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
    ; public_description
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
      ; description = public_description
      ; location
      ; max_participants
      ; min_participants
      ; overbook
      ; assignment_count
      ; canceled_at
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
    ; custom_fields = None
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
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

  let create_manual_message
    ?(recipient = "foo@bar.com" |> Pool_user.EmailAddress.of_string)
    ()
    =
    let open Message_template in
    Message_template.ManualMessage.
      { recipient
      ; language = Pool_common.Language.En
      ; email_subject = EmailSubject.of_string "subject"
      ; email_text = EmailText.of_string "<p>hello</p>"
      ; plain_text = PlainText.of_string "hellp"
      }
  ;;
end

module FilterHelper = struct
  let equal = Filter.Operator.(Equality.Equal |> equality)
  let contains = Filter.Operator.(ListM.ContainsAll |> list)
end

module Repo = struct
  let first_contact () =
    let open CCFun.Infix in
    let open Utils.Lwt_result.Infix in
    Contact.find_all Data.database_label () ||> fst %> CCList.hd
  ;;

  let first_tag () =
    let open Utils.Lwt_result.Infix in
    Tags.find_by Data.database_label ||> fst ||> CCList.hd
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

let truncate_tables database_label tables =
  let open Utils.Lwt_result.Infix in
  let open Caqti_request.Infix in
  let query name =
    [%string "TRUNCATE TABLE %{name}"] |> Caqti_type.(unit ->. unit)
  in
  Database.with_disabled_fk_check database_label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    CCList.map query tables
    |> Lwt_list.map_s (CCFun.flip Connection.exec ())
    ||> CCResult.flatten_l)
  ||> Utils.flat_unit
;;

let case
  ?(preparation : unit -> (unit, Pool_message.Error.t) Lwt_result.t =
    fun () -> Lwt.return_ok ())
  ?(cleanup : unit -> (unit, Pool_message.Error.t) Lwt_result.t =
    fun () -> Lwt.return_ok ())
  fn
  (_switch : Lwt_switch.t)
  ()
  : unit Lwt.t
  =
  let open Utils.Lwt_result.Infix in
  Lwt.map Pool_common.Utils.get_or_failwith
  @@
  let* () = preparation () in
  let* () = fn () in
  cleanup ()
;;
