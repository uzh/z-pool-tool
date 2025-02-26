let hour = Ptime.Span.of_int_s @@ (60 * 60)

let create_contact () =
  let open Pool_common in
  let open Pool_user in
  Contact.
    { user =
        { Pool_user.id = Pool_user.Id.create ()
        ; email = EmailAddress.of_string "jane.doe@econ.uzh.ch"
        ; lastname = Lastname.of_string "Doe"
        ; firstname = Firstname.of_string "Jane"
        ; status = Status.Active
        ; admin = IsAdmin.create false
        ; confirmed = Confirmed.create true
        }
    ; terms_accepted_at = TermsAccepted.create_now () |> CCOption.pure
    ; language = Some Language.En
    ; experiment_type_preference = None
    ; cell_phone = Some (Pool_user.CellPhone.of_string "+41791234567")
    ; paused = Paused.create false
    ; disabled = Disabled.create false
    ; verified = None
    ; email_verified = () |> Ptime_clock.now |> EmailVerified.create |> CCOption.pure
    ; num_invitations = NumberOfInvitations.init
    ; num_assignments = NumberOfAssignments.init
    ; num_show_ups = NumberOfShowUps.init
    ; num_no_shows = NumberOfNoShows.init
    ; num_participations = NumberOfParticipations.init
    ; firstname_version = Version.create ()
    ; lastname_version = Version.create ()
    ; paused_version = Version.create ()
    ; language_version = Version.create ()
    ; experiment_type_preference_version = Version.create ()
    ; import_pending = Pool_user.ImportPending.create false
    ; created_at = CreatedAt.create_now ()
    ; updated_at = UpdatedAt.create_now ()
    }
;;

let create_user () = () |> create_contact |> fun { Contact.user; _ } -> user

let location =
  let open Pool_location in
  let open CCResult in
  let get_exn = CCResult.get_exn in
  let address =
    let open Address in
    let mail =
      let open Mail in
      let* institution = "USZ" |> Institution.create in
      let* room = "SNS Lab" |> Room.create in
      let* building = "University Hospital Zurich" |> Building.create in
      let* street = "Rämistrasse 100" |> Street.create in
      let* zip = "8091" |> Zip.create in
      let* city = "Zurich" |> City.create in
      Ok
        { institution = Some institution
        ; room = Some room
        ; building = Some building
        ; street
        ; zip
        ; city
        }
    in
    let mail = mail |> get_exn in
    Physical mail
  in
  let name = "SNS Lab" |> Name.create |> get_exn in
  let description =
    let text_en =
      "The Laboratory for Social and Neural Systems Research (SNS Lab) is the heart of \
       the ZNE."
    in
    let text_de =
      "Das Labor für Soziale und Neuronale Systeme (SNS Lab) ist das Herzstück des ZNE."
    in
    let languages = Pool_common.Language.all in
    Pool_common.Language.[ En, text_en; De, text_de ]
    |> Description.create languages
    |> get_exn
    |> CCOption.return
  in
  let address = address in
  let link = "https://www.zne.uzh.ch/en/facilities.html" |> Link.create |> get_exn in
  let status = Status.Active in
  let files = [] in
  { id = Id.create ()
  ; name
  ; description
  ; address
  ; link = Some link
  ; status
  ; files
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

let create_session experiment =
  let get_or_failwith = Pool_common.Utils.get_or_failwith in
  Session.
    { id = Session.Id.create ()
    ; follow_up_to = None
    ; has_follow_ups = false
    ; start =
        Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid start"
        |> Start.create
    ; duration = Duration.create hour |> get_or_failwith
    ; internal_description =
        "Internal description" |> InternalDescription.of_string |> CCOption.return
    ; public_description =
        "Public description" |> PublicDescription.of_string |> CCOption.return
    ; location
    ; max_participants = ParticipantAmount.create 30 |> get_or_failwith
    ; min_participants = ParticipantAmount.create 1 |> get_or_failwith
    ; overbook = ParticipantAmount.create 4 |> get_or_failwith
    ; assignment_count = 0 |> AssignmentCount.create |> get_or_failwith
    ; no_show_count = 0 |> NoShowCount.create |> get_or_failwith
    ; participant_count = 0 |> ParticipantCount.create |> get_or_failwith
    ; email_reminder_lead_time = None
    ; email_reminder_sent_at = None
    ; text_message_reminder_lead_time = None
    ; text_message_reminder_sent_at = None
    ; closed_at = None
    ; canceled_at = None
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    ; experiment
    }
;;

let create_experiment () =
  let get_exn = CCResult.get_exn in
  let open Experiment in
  let online_experiment =
    OnlineExperiment.
      { survey_url = SurveyUrl.of_string "https://www.qualtics.com/T8rp6WTdk" }
  in
  { id = Id.create ()
  ; title = Title.create "The Wallet Game\t" |> get_exn
  ; public_title = PublicTitle.create "public_title" |> get_exn
  ; internal_description =
      InternalDescription.create "An internal description" |> get_exn |> CCOption.return
  ; public_description =
      PublicDescription.create "A description for everyone" |> get_exn |> CCOption.return
  ; language = None
  ; organisational_unit = None
  ; smtp_auth_id = None
  ; cost_center = Some ("A-11111-22-33" |> CostCenter.of_string)
  ; filter = None
  ; contact_email = None
  ; direct_registration_disabled = false |> DirectRegistrationDisabled.create
  ; registration_disabled = false |> RegistrationDisabled.create
  ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
  ; external_data_required = false |> ExternalDataRequired.create
  ; show_external_data_id_links = false |> ShowExternalDataIdLinks.create
  ; experiment_type = Some Pool_common.ExperimentType.Lab
  ; online_experiment = Some online_experiment
  ; email_session_reminder_lead_time = None
  ; text_message_session_reminder_lead_time = None
  ; matcher_notification_sent = MatcherNotificationSent.create false
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

let create_assignment ?contact () =
  let open Assignment in
  let contact = CCOption.value ~default:(create_contact ()) contact in
  { id = Id.create ()
  ; contact
  ; no_show = Some (false |> NoShow.create)
  ; participated = Some (false |> Participated.create)
  ; matches_filter = MatchesFilter.init
  ; canceled_at = None
  ; marked_as_deleted = MarkedAsDeleted.init
  ; external_data_id = Some (ExternalDataId.of_string "DATA_ID")
  ; reminder_manually_last_sent_at = None
  ; custom_fields = None
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;
