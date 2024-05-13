open Tyxml.Html
module DummyData = Component_message_text_elements_data

let message_template_hints =
  let open Message_template.Label in
  let open Pool_common.I18n in
  function
  | ExperimentInvitation ->
    [ "experumentSurveyRedirectUrl", ExperumentSurveyRedirectUrl ]
  | AccountSuspensionNotification
  | AssignmentCancellation
  | AssignmentConfirmation
  | AssignmentSessionChange
  | ContactEmailChangeAttempt
  | ContactRegistrationAttempt
  | EmailVerification
  | ManualSessionMessage
  | MatcherNotification
  | MatchFilterUpdateNotification
  | PasswordChange
  | PasswordReset
  | PhoneVerification
  | ProfileUpdateTrigger
  | SignUpVerification
  | SessionCancellation
  | SessionReminder
  | SessionReschedule
  | UserImport
  | WaitingListConfirmation -> []
;;

let build_help ?(hints = []) language help =
  let open Pool_common in
  let title =
    Utils.text_to_string language I18n.TextTemplates
    |> CCString.capitalize_ascii
  in
  let help =
    help
    |> CCList.map (fun (elm, example) ->
      let placeholder = Format.asprintf "{%s}" elm in
      tr
        [ td
            [ span
                ~a:[ a_user_data "clipboard" placeholder ]
                [ txt placeholder ]
            ]
        ; td [ Http_utils.add_line_breaks example ]
        ])
  in
  let hints =
    match hints with
    | [] -> []
    | hints ->
      let hints =
        let cell html = td [ div ~a:[ a_class [ "help" ] ] html ] in
        hints
        |> CCList.map (fun (label, hint) ->
          tr
            [ cell [ txt label ]
            ; cell [ p [ Unsafe.data (Utils.hint_to_string language hint) ] ]
            ])
      in
      let subtitle =
        tr
          [ td
              ~a:[ a_colspan 2 ]
              [ div
                  ~a:[ a_class [ "gap"; "flexcolumn" ] ]
                  [ strong [ txt (Utils.text_to_string language I18n.Help) ] ]
              ]
          ]
      in
      subtitle :: hints
  in
  help @ hints
  |> table ~a:[ a_class [ "table"; "simple"; "align-top" ] ]
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
  let hints = message_template_hints template_label in
  build_help ~hints language
  @@
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
    ContactEmailChangeAttempt.email_params layout tenant_url (create_user ())
  | ContactRegistrationAttempt ->
    let tenant_url = tenant.Pool_tenant.url in
    ContactRegistrationAttempt.email_params layout tenant_url (create_user ())
  | EmailVerification ->
    let validation_url =
      [ Pool_message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/email-verified"
    in
    EmailVerification.email_params layout validation_url (create_contact ())
  | ExperimentInvitation ->
    ExperimentInvitation.email_params
      layout
      (create_experiment ())
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
      (create_user ())
      (create_experiment ())
  | MatchFilterUpdateNotification ->
    let session = create_session () in
    let assignment = create_assignment () in
    MatchFilterUpdateNotification.email_params
      layout
      (create_user ())
      (create_experiment ())
      [ session, [ assignment ] ]
  | PasswordChange -> PasswordChange.email_params layout (create_user ())
  | PasswordReset ->
    let reset_url =
      [ Pool_message.Field.Token, Email.Token.value token ]
      |> create_public_url_with_params tenant.Pool_tenant.url "/reset-password/"
    in
    PasswordReset.email_params layout reset_url (create_user ())
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
      [ Pool_message.Field.Token, Email.Token.value token ]
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
      [ Pool_message.Field.Token, Email.Token.value token ]
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

let online_survey_hints =
  [ "callbackUrl", Pool_common.I18n.ExperimentCallbackUrl ]
;;

let online_survey_help tenant ?experiment () =
  let open DummyData in
  let assignment = create_assignment () in
  let experiment = CCOption.value ~default:(create_experiment ()) experiment in
  Experiment.OnlineExperiment.url_params
    tenant
    ~experiment_id:experiment.Experiment.id
    ~assignment_id:Assignment.(assignment.id |> Id.to_common)
;;
