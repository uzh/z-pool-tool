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
      ; paused = Paused.create false
      ; disabled = Disabled.create false
      ; verified = None
      ; email_verified =
          () |> Ptime_clock.now |> EmailVerified.create |> CCOption.pure
      ; num_invitations = NumberOfInvitations.init
      ; num_assignments = NumberOfAssignments.init
      ; num_show_ups = NumberOfShowUps.init
      ; num_participations = NumberOfParticipations.init
      ; firstname_version = Version.create ()
      ; lastname_version = Version.create ()
      ; paused_version = Version.create ()
      ; language_version = Version.create ()
      ; experiment_type_preference_version = Version.create ()
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
          ; room
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
    let address = address in
    let link =
      "https://www.zne.uzh.ch/en/facilities.html" |> Link.create |> get_exn
    in
    let status = Status.Active in
    let files = [] in
    { id = Id.create ()
    ; name
    ; description = None
    ; address
    ; link = Some link
    ; status
    ; files
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
  ;;

  let create_session () =
    Session.
      { id = Pool_common.Id.create ()
      ; follow_up_to = None
      ; start =
          Ptime.add_span (Ptime_clock.now ()) hour
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      ; duration = Duration.create hour |> Pool_common.Utils.get_or_failwith
      ; description = None
      ; location
      ; max_participants =
          ParticipantAmount.create 30 |> Pool_common.Utils.get_or_failwith
      ; min_participants =
          ParticipantAmount.create 1 |> Pool_common.Utils.get_or_failwith
      ; overbook =
          ParticipantAmount.create 4 |> Pool_common.Utils.get_or_failwith
      ; reminder_subject = None
      ; reminder_lead_time = None
      ; reminder_text = None
      ; reminder_sent_at = None
      ; assignment_count =
          0 |> AssignmentCount.create |> Pool_common.Utils.get_or_failwith
      ; closed_at = None
      ; canceled_at = None
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
  ;;

  let create_experiment () =
    let get_exn = CCResult.get_exn in
    Experiment.
      { id = Id.create ()
      ; title = Title.create "The Wallet Game\t" |> get_exn
      ; public_title = PublicTitle.create "public_title" |> get_exn
      ; description = Description.create "A description for everyone" |> get_exn
      ; filter = None
      ; invitation_template = None
      ; session_reminder_subject = None
      ; session_reminder_text = None
      ; session_reminder_lead_time = None
      ; direct_registration_disabled =
          false |> DirectRegistrationDisabled.create
      ; registration_disabled = false |> RegistrationDisabled.create
      ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
      ; experiment_type = Some Pool_common.ExperimentType.Lab
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
  ;;

  let name_element = "name", div [ txt "John Doe" ]
end

let build_help language toggle_id help =
  let wrap_hints html =
    div
      ~a:[ a_class [ "card" ] ]
      [ div
          ~a:[ a_class [ "card-header" ] ]
          [ label
              ~a:[ a_label_for toggle_id; a_class [ "flexrow"; "flex-gap-xs" ] ]
              [ strong
                  [ txt
                      Pool_common.(
                        Utils.text_to_string language I18n.TextTemplates
                        |> CCString.capitalize_ascii)
                  ]
              ; Component_icon.icon `HelpOutline
              ]
          ]
      ; input
          ~a:[ a_input_type `Checkbox; a_class [ "toggle" ]; a_id toggle_id ]
          ()
      ; div
          ~a:[ a_class [ "toggle-body"; "card-body" ] ]
          [ p
              [ txt
                  Pool_common.(
                    Utils.hint_to_string language I18n.TemplateTextElementsHint)
              ]
          ; html
          ]
      ]
  in
  help
  |> CCList.map (fun (elm, example) ->
       [ txt (Format.asprintf "{%s}" elm); example ])
  |> Component_table.horizontal_table `Simple ~align_top:true
  |> wrap_hints
;;

let message_template_help
  language
  (tenant : Pool_tenant.t)
  ?contact
  ?experiment
  ?session
  template_label
  =
  let open Message_template in
  let open Label in
  let open DummyData in
  let token = "123456789" |> Email.Token.create in
  let open CCOption in
  let create_contact () = value ~default:(create_contact ()) contact in
  let create_experiment () = value ~default:(create_experiment ()) experiment in
  let create_session () = value ~default:(create_session ()) session in
  match template_label with
  | AssignmentConfirmation ->
    AssignmentConfirmation.email_params
      language
      (create_session ())
      (create_contact ())
  | EmailVerification ->
    let validation_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/email-verified"
    in
    EmailVerification.email_params validation_url (create_contact ())
  | ExperimentInvitation ->
    ExperimentInvitation.email_params
      (create_experiment ())
      tenant.Pool_tenant.url
      (create_contact ())
  | PasswordChange -> PasswordChange.email_params (() |> create_sihl_user)
  | PasswordReset ->
    let reset_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/reset-password/"
    in
    PasswordReset.email_params reset_url (create_sihl_user ())
  | ProfileUpdateTrigger ->
    ProfileUpdateTrigger.email_params tenant.Pool_tenant.url (create_contact ())
  | SessionCancellation ->
    let reason =
      "Experiment assistant is sick"
      |> Session.CancellationReason.create
      |> CCResult.get_exn
    in
    SessionCancellation.email_params
      language
      (create_session ())
      reason
      (create_contact ())
  | SessionReminder ->
    SessionReminder.email_params
      language
      (create_experiment ())
      (create_session ())
      (create_contact ())
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
      verification_url
      (Contact.firstname contact)
      (Contact.lastname contact)
;;

let session_reminder_help language sys_languages ?session () =
  let session = CCOption.value ~default:(DummyData.create_session ()) session in
  let session_overview =
    (CCList.map (fun lang ->
       ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
       , Session.(to_email_text lang session) |> Http_utils.add_line_breaks )))
      sys_languages
  in
  DummyData.name_element :: session_overview
  |> build_help language "session-reminder-help"
;;

let experiment_invitation_help language ?experiment () =
  let experiment =
    CCOption.value ~default:DummyData.(create_experiment ()) experiment
  in
  let text_elements =
    Invitation.email_experiment_elements experiment
    |> CCList.map (fun (label, content) -> label, div [ txt content ])
  in
  DummyData.name_element :: text_elements
  |> build_help language "experiment-invitation-help"
;;
