open Tyxml.Html

module DummyData = struct
  let hour = Ptime.Span.of_int_s @@ (60 * 60)

  let create_contact () =
    let open Pool_common in
    let open Pool_user in
    Contact.
      { user =
          Sihl_user.
            { id = Id.(create () |> value)
            ; email = "jane.doe@econ.uzh.ch"
            ; username = None
            ; name = Some "Doe"
            ; given_name = Some "Jane"
            ; password =
                "somepassword"
                |> Sihl_user.Hashing.hash
                |> CCResult.get_or_failwith
            ; status =
                Sihl_user.status_of_string "active" |> CCResult.get_or_failwith
            ; admin = false
            ; confirmed = true
            ; created_at = CreatedAt.create ()
            ; updated_at = UpdatedAt.create ()
            }
      ; terms_accepted_at = TermsAccepted.create_now () |> CCOption.pure
      ; language = Some Language.En
      ; experiment_type_preference = None
      ; cell_phone = Some (Pool_user.CellPhone.of_string "+41791234567")
      ; paused = Paused.create false
      ; disabled = Disabled.create false
      ; verified = None
      ; email_verified =
          () |> Ptime_clock.now |> EmailVerified.create |> CCOption.pure
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
      ; created_at = CreatedAt.create ()
      ; updated_at = UpdatedAt.create ()
      }
  ;;

  let create_sihl_user () =
    () |> create_contact |> fun { Contact.user; _ } -> user
  ;;

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
        "The Laboratory for Social and Neural Systems Research (SNS Lab) is \
         the heart of the ZNE."
      in
      let text_de =
        "Das Labor für Soziale und Neuronale Systeme (SNS Lab) ist das \
         Herzstück des ZNE."
      in
      let languages = Pool_common.Language.all in
      Pool_common.Language.[ En, text_en; De, text_de ]
      |> Description.create languages
      |> get_exn
      |> CCOption.return
    in
    let address = address in
    let link =
      "https://www.zne.uzh.ch/en/facilities.html" |> Link.create |> get_exn
    in
    let status = Status.Active in
    let files = [] in
    { id = Id.create ()
    ; name
    ; description
    ; address
    ; link = Some link
    ; status
    ; files
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
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
          "Internal description"
          |> InternalDescription.of_string
          |> CCOption.return
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
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      ; experiment
      }
  ;;

  let create_experiment () =
    let get_exn = CCResult.get_exn in
    Experiment.
      { id = Id.create ()
      ; title = Title.create "The Wallet Game\t" |> get_exn
      ; public_title = PublicTitle.create "public_title" |> get_exn
      ; internal_description =
          InternalDescription.create "An internal description"
          |> get_exn
          |> CCOption.return
      ; public_description =
          PublicDescription.create "A description for everyone"
          |> get_exn
          |> CCOption.return
      ; language = None
      ; organisational_unit = None
      ; smtp_auth_id = None
      ; cost_center = Some ("A-11111-22-33" |> CostCenter.of_string)
      ; filter = None
      ; contact_email = None
      ; direct_registration_disabled =
          false |> DirectRegistrationDisabled.create
      ; registration_disabled = false |> RegistrationDisabled.create
      ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
      ; external_data_required = false |> ExternalDataRequired.create
      ; show_external_data_id_links = false |> ShowExternalDataIdLinks.create
      ; experiment_type = Some Pool_common.ExperimentType.Lab
      ; assignment_without_session = false |> AssignmentWithoutSession.create
      ; redirect_immediately = false |> RedirectImmediately.create
      ; email_session_reminder_lead_time = None
      ; text_message_session_reminder_lead_time = None
      ; invitation_reset_at = None
      ; matcher_notification_sent = MatcherNotificationSent.create false
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
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
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
  ;;

  let name_element = "name", div [ txt "John Doe" ]
end

let build_help language help =
  let title =
    Pool_common.(
      Utils.text_to_string language I18n.TextTemplates
      |> CCString.capitalize_ascii)
  in
  help
  |> CCList.map (fun (elm, example) ->
    let placeholder = Format.asprintf "{%s}" elm in
    [ span ~a:[ a_user_data "clipboard" placeholder ] [ txt placeholder ]
    ; Http_utils.add_line_breaks example
    ])
  |> Component_table.horizontal_table `Simple ~align_top:true
  |> Component_collapsible.create_note ~icon:None ~title language
;;

let message_template_help
  language
  (tenant : Pool_tenant.t)
  ?contact
  ?experiment
  ?session
  ?assignment
  template_label
  =
  let open Message_template in
  let open Label in
  let open DummyData in
  let token = "123456789" |> Email.Token.create in
  let open CCOption in
  let create_contact () = value ~default:(create_contact ()) contact in
  let create_experiment () = value ~default:(create_experiment ()) experiment in
  let create_public_experiment () =
    create_experiment () |> Experiment.to_public
  in
  let create_session () =
    value ~default:(create_session (create_experiment ())) session
  in
  let create_follow_up session_id =
    Session.{ (create_session ()) with follow_up_to = Some session_id }
  in
  let create_assignment () =
    value ~default:(create_assignment ?contact ()) assignment
  in
  let layout = layout_from_tenant tenant in
  match template_label with
  | AccountSuspensionNotification ->
    let contact = create_contact () in
    AccountSuspensionNotification.email_params layout contact.Contact.user
  | AssignmentCancellation ->
    let session = create_session () in
    AssignmentCancellation.email_params
      ~follow_up_sessions:[ create_follow_up session.Session.id ]
      language
      layout
      (create_experiment ())
      session
      (create_assignment ())
  | AssignmentConfirmation ->
    let session = create_session () in
    AssignmentConfirmation.email_params
      ~follow_up_sessions:[ create_follow_up session.Session.id ]
      language
      layout
      (create_experiment ())
      session
      (create_assignment ())
  | AssignmentSessionChange ->
    let session = create_session () in
    AssignmentSessionChange.email_params
      language
      layout
      (create_experiment ())
      ~new_session:session
      ~old_session:session
      (create_assignment ())
  | ContactEmailChangeAttempt ->
    let tenant_url = tenant.Pool_tenant.url in
    ContactEmailChangeAttempt.email_params
      layout
      tenant_url
      (create_sihl_user ())
  | ContactRegistrationAttempt ->
    let tenant_url = tenant.Pool_tenant.url in
    ContactRegistrationAttempt.email_params
      layout
      tenant_url
      (create_sihl_user ())
  | EmailVerification ->
    let validation_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/email-verified"
    in
    EmailVerification.email_params layout validation_url (create_contact ())
  | ExperimentInvitation ->
    ExperimentInvitation.email_params
      layout
      (create_experiment ())
      tenant.Pool_tenant.url
      (create_contact ())
  | ManualSessionMessage ->
    ManualSessionMessage.email_params
      language
      layout
      (create_experiment ())
      (create_session ())
      (create_assignment ())
  | MatcherNotification ->
    MatcherNotification.email_params
      layout
      (create_sihl_user ())
      (create_experiment ())
  | MatchFilterUpdateNotification ->
    let session = create_session () in
    let assignment = create_assignment () in
    MatchFilterUpdateNotification.email_params
      layout
      (create_sihl_user ())
      (create_experiment ())
      [ session, [ assignment ] ]
  | PasswordChange -> PasswordChange.email_params layout (create_sihl_user ())
  | PasswordReset ->
    let reset_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/reset-password/"
    in
    PasswordReset.email_params layout reset_url (create_sihl_user ())
  | PhoneVerification ->
    let code = Pool_common.VerificationCode.create () in
    PhoneVerification.message_params code
  | ProfileUpdateTrigger ->
    ProfileUpdateTrigger.email_params
      layout
      tenant.Pool_tenant.url
      (create_contact ())
  | SessionCancellation ->
    let follow_up_sessions =
      let open Session in
      let follow_up = create_session () in
      let start =
        Ptime.add_span
          (follow_up.start |> Start.value)
          (Ptime.Span.of_int_s 3600)
        |> CCOption.get_exn_or "Invalid timespan provided"
        |> Start.create
      in
      [ Session.{ follow_up with start } ]
    in
    let reason =
      "Experiment assistant is sick"
      |> Session.CancellationReason.create
      |> CCResult.get_exn
    in
    SessionCancellation.email_params
      language
      layout
      (create_experiment ())
      (create_session ())
      follow_up_sessions
      reason
      (create_contact ())
  | SessionReminder ->
    SessionReminder.email_params
      language
      layout
      (create_experiment ())
      (create_session ())
      (create_assignment ())
  | SessionReschedule ->
    let open Session in
    let start =
      Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid start"
      |> Start.create
    in
    let duration = hour |> Duration.create |> CCResult.get_exn in
    SessionReschedule.email_params
      language
      layout
      (create_experiment ())
      (create_session ())
      start
      duration
      (create_contact ())
  | SignUpVerification ->
    let verification_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/email-verified"
    in
    let contact = create_contact () in
    SignUpVerification.email_params
      layout
      verification_url
      (Contact.firstname contact)
      (Contact.lastname contact)
  | UserImport ->
    let confirmation_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params
           tenant.Pool_tenant.url
           "/import-confirmation"
    in
    let contact = create_contact () in
    UserImport.email_params layout confirmation_url (`Contact contact)
  | WaitingListConfirmation ->
    WaitingListConfirmation.email_params
      layout
      (create_contact ())
      (create_public_experiment ())
;;
