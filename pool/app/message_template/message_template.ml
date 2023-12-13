include Entity
include Event
include Default
include Message_utils
module Guard = Entity_guard
open CCFun.Infix

let src = Logs.Src.create "message_template"
let find = Repo.find

let find_default_by_label_and_language pool language label =
  let open Utils.Lwt_result.Infix in
  Repo.find_default_by_label_and_language pool language label
  ||> CCOption.get_exn_or
        Pool_common.(
          Utils.error_to_string
            Language.En
            Message.(NotFound Field.MessageTemplate))
;;

let all_default = Repo.all_default
let find_all_of_entity_by_label = Repo.find_all_of_entity_by_label
let find_by_label_to_send = Repo.find_by_label_to_send
let find_all_by_label_to_send = Repo.find_all_by_label_to_send
let default_sender_of_pool = Email.Service.default_sender_of_pool

let sender_of_contact_person pool admin =
  match admin with
  | None -> default_sender_of_pool pool
  | Some admin -> admin |> Admin.email_address |> Lwt.return
;;

let to_absolute_path layout path =
  path |> Sihl.Web.externalize_path |> Format.asprintf "%s%s" layout.link
;;

let sender_of_experiment pool experiment =
  let open Utils.Lwt_result.Infix in
  Experiment.find_contact_person pool experiment
  >|> CCOption.map_or
        ~default:(default_sender_of_pool pool)
        (Admin.email_address %> Lwt.return)
;;

let sender_of_public_experiment pool { Experiment.Public.id; _ } =
  let open Utils.Lwt_result.Infix in
  Experiment.find pool id |>> sender_of_experiment pool
;;

let filter_languages ?(exclude = []) available templates =
  let exclude =
    exclude @ (templates |> CCList.map (fun { language; _ } -> language))
  in
  available |> CCList.filter CCFun.(flip CCList.mem exclude %> not)
;;

let missing_template_languages database_label entity_id label ?exclude languages
  =
  let%lwt existing =
    find_all_of_entity_by_label database_label entity_id label
  in
  filter_languages ?exclude languages existing |> Lwt.return
;;

let prepare_email language template sender email layout params =
  let open Sihl_email in
  let { Entity.email_subject; email_text; plain_text; _ } = template in
  let mail =
    { sender = Pool_user.EmailAddress.value sender
    ; recipient = Pool_user.EmailAddress.value email
    ; subject = email_subject
    ; text = PlainText.value plain_text
    ; html = Some (combine_html language (Some email_subject))
    ; cc = []
    ; bcc = []
    }
  in
  let params = [ "emailText", email_text ] @ layout_params layout @ params in
  Message_utils.render_email_params params mail
;;

(* TODO: Should this be using experiment language as well? *)
let prepare_manual_email
  { ManualMessage.recipient; language; email_subject; email_text; plain_text }
  layout
  params
  sender
  =
  let open Sihl_email in
  let mail =
    { sender = Pool_user.EmailAddress.value sender
    ; recipient = Pool_user.EmailAddress.value recipient
    ; subject = email_subject
    ; text = PlainText.value plain_text
    ; html = Some (combine_html language (Some email_subject))
    ; cc = []
    ; bcc = []
    }
  in
  let params = [ "emailText", email_text ] @ layout_params layout @ params in
  Message_utils.render_email_params params mail
;;

let global_params layout user =
  Pool_user.
    [ "name", user |> user_fullname
    ; "firstname", user |> user_firstname |> Firstname.value
    ; "lastname", user |> user_lastname |> Lastname.value
    ; "siteTitle", layout.site_title
    ; "siteUrl", layout.link
    ]
;;

let public_experiment_params
  layout
  { Experiment.Public.id; public_title; description; _ }
  =
  let open Experiment in
  let experiment_id = id |> Id.value in
  let experiment_url =
    Format.asprintf "experiments/%s" experiment_id |> to_absolute_path layout
  in
  [ "experimentId", experiment_id
  ; "experimentPublicTitle", PublicTitle.value public_title
  ; ( "experimentDescription"
    , description |> CCOption.map_or ~default:"" Description.value )
  ; "experimentUrl", experiment_url
  ]
;;

let experiment_params layout experiment =
  public_experiment_params layout (Experiment.to_public experiment)
;;

let location_params
  language
  layout
  ({ Pool_location.id; address; description; _ } as location)
  =
  let open Pool_location in
  let location_url =
    id |> Id.value |> Format.asprintf "location/%s" |> to_absolute_path layout
  in
  let location_link = Human.link_with_default ~default:location_url location in
  let location_details = Human.detailed language location in
  let location_description =
    CCOption.bind description (Description.find_opt language)
    |> CCOption.value ~default:""
  in
  let institution, building, room, street, zip, city =
    let open Address in
    match address with
    | Virtual -> "", "", "", "", "", ""
    | Physical { Mail.institution; building; room; street; zip; city } ->
      let open Mail in
      let default fnc = CCOption.map_or ~default:"" fnc in
      let institution = institution |> default Institution.value in
      let building = building |> default Building.value in
      let room = room |> default Room.value in
      let street = street |> Street.value in
      let zip = zip |> Zip.value in
      let city = city |> City.value in
      institution, building, room, street, zip, city
  in
  [ "locationUrl", location_url
  ; "locationDetails", location_details
  ; "locationLink", location_link
  ; "locationInstitution", institution
  ; "locationBuilding", building
  ; "locationRoom", room
  ; "locationStreet", street
  ; "locationZip", zip
  ; "locationCity", city
  ; "locationDescription", location_description
  ]
;;

let session_params
  layout
  ?follow_up_sessions
  ?prefix
  lang
  ({ Session.start; duration; location; _ } as session : Session.t)
  =
  let open Session in
  let session_id = session.Session.id |> Id.value in
  let session_overview =
    let main_session = Session.to_email_text lang session in
    match follow_up_sessions with
    | None | Some ([], _) -> main_session
    | Some (sessions, i18n) ->
      let follow_ups =
        [ Pool_common.(Utils.hint_to_string lang i18n)
        ; follow_up_sessions_to_email_list sessions
        ]
        |> CCString.concat "\n"
      in
      [ main_session; follow_ups ] |> CCString.concat "\n\n"
  in
  let start =
    start |> Start.value |> Pool_common.Utils.Time.formatted_date_time
  in
  let duration =
    duration |> Duration.value |> Pool_common.Utils.Time.formatted_timespan
  in
  let session_params =
    [ "sessionId", session_id
    ; "sessionStart", start
    ; "sessionDateTime", Session.start_end_to_human session
    ; "sessionDuration", duration
    ; "sessionOverview", session_overview
    ]
  in
  match prefix with
  | None -> session_params @ location_params lang layout location
  | Some prefix ->
    session_params
    |> CCList.map (fun (label, value) ->
      Format.asprintf "%s%s" prefix (CCString.capitalize_ascii label), value)
;;

let assignment_params { Assignment.id; external_data_id; _ } =
  let open Assignment in
  let external_data_id =
    CCOption.map_or ExternalDataId.value ~default:"" external_data_id
  in
  let assignment_id = Id.value id in
  [ "assignmentId", assignment_id; "externalDataId", external_data_id ]
;;

module AccountSuspensionNotification = struct
  let email_params = global_params

  let create ({ Pool_tenant.database_label; _ } as tenant) user =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let open Sihl.Contract in
    let%lwt system_languages = Settings.find_languages database_label in
    let email = user.User.email |> Pool_user.EmailAddress.of_string in
    let* preferred_langauge =
      match user.User.admin with
      | true -> Lwt_result.return Pool_common.Language.En
      | false ->
        email
        |> Contact.find_by_email database_label
        >|+ contact_language system_languages
    in
    let%lwt template, language =
      find_by_label_to_send
        database_label
        preferred_langauge
        Label.AccountSuspensionNotification
    in
    let%lwt sender = default_sender_of_pool database_label in
    let layout = layout_from_tenant tenant in
    let params = email_params layout user in
    prepare_email language template sender email layout params
    |> Lwt_result.return
  ;;
end

module AssignmentConfirmation = struct
  open Assignment

  let base_params layout contact = contact.Contact.user |> global_params layout

  let email_params
    ?follow_up_sessions
    language
    layout
    experiment
    session
    assignment
    =
    let follow_up_sessions =
      CCOption.map
        (fun lst ->
          lst, Pool_common.I18n.AssignmentConfirmationMessageFollowUps)
        follow_up_sessions
    in
    base_params layout assignment.contact
    @ experiment_params layout experiment
    @ session_params ?follow_up_sessions layout language session
    @ assignment_params assignment
  ;;

  let template pool experiment language =
    find_by_label_to_send
      ~entity_uuids:[ Experiment.Id.to_common experiment.Experiment.id ]
      pool
      language
      Label.AssignmentConfirmation
  ;;

  let prepare
    ?follow_up_sessions
    pool
    tenant
    contact
    experiment
    session
    admin_contact
    =
    let%lwt sys_langs = Settings.find_languages pool in
    let%lwt template, language =
      experiment_message_language sys_langs experiment contact
      |> template pool experiment
    in
    let layout = layout_from_tenant tenant in
    let%lwt sender = sender_of_contact_person pool admin_contact in
    let fnc assignment =
      let params =
        email_params
          ?follow_up_sessions
          language
          layout
          experiment
          session
          assignment
      in
      let email = assignment.contact |> Contact.email_address in
      prepare_email language template sender email layout params
    in
    Lwt.return fnc
  ;;
end

module AssignmentSessionChange = struct
  let base_params layout contact = contact.Contact.user |> global_params layout

  let email_params
    language
    layout
    experiment
    ~new_session
    ~old_session
    assignment
    =
    base_params layout assignment.Assignment.contact
    @ experiment_params layout experiment
    @ session_params layout language new_session
    @ session_params ~prefix:"old" layout language old_session
    @ assignment_params assignment
  ;;

  let create pool message tenant experiment ~new_session ~old_session assignment
    =
    let layout = layout_from_tenant tenant in
    let%lwt sender = sender_of_experiment pool experiment in
    let params =
      email_params
        message.ManualMessage.language
        layout
        experiment
        ~new_session
        ~old_session
        assignment
    in
    prepare_manual_email message layout params sender |> Lwt.return
  ;;
end

module ContactEmailChangeAttempt = struct
  let email_params layout tenant_url user =
    let reset_url = create_public_url tenant_url "/request-reset-password" in
    global_params layout user
    @ [ "tenantUrl", Pool_tenant.Url.value tenant_url
      ; "resetUrl", reset_url
      ; ( "emailAddress"
        , Pool_user.EmailAddress.value (Pool_user.user_email_address user) )
      ]
  ;;

  let create pool tenant user =
    let open Utils.Lwt_result.Infix in
    let* message_language =
      let%lwt sys_langs = Settings.find_languages pool in
      match%lwt Admin.user_is_admin pool user with
      | true -> Lwt_result.return Pool_common.Language.En
      | false ->
        let* contact = Contact.find_by_user pool user in
        contact_language sys_langs contact |> Lwt_result.return
    in
    let%lwt template, language =
      find_by_label_to_send
        pool
        message_language
        Label.ContactEmailChangeAttempt
    in
    let layout = layout_from_tenant tenant in
    let tenant_url = tenant.Pool_tenant.url in
    let%lwt sender = default_sender_of_pool pool in
    prepare_email
      language
      template
      sender
      (Pool_user.user_email_address user)
      layout
      (email_params layout tenant_url user)
    |> Lwt_result.return
  ;;
end

module ContactRegistrationAttempt = struct
  let email_params layout tenant_url user =
    let reset_url = create_public_url tenant_url "/request-reset-password" in
    global_params layout user
    @ [ "tenantUrl", Pool_tenant.Url.value tenant_url
      ; "resetUrl", reset_url
      ; ( "emailAddress"
        , Pool_user.EmailAddress.value (Pool_user.user_email_address user) )
      ]
  ;;

  let create pool message_language tenant user =
    let%lwt template, language =
      find_by_label_to_send
        pool
        message_language
        Label.ContactRegistrationAttempt
    in
    let layout = layout_from_tenant tenant in
    let tenant_url = tenant.Pool_tenant.url in
    let%lwt sender = default_sender_of_pool pool in
    prepare_email
      language
      template
      sender
      (Pool_user.user_email_address user)
      layout
      (email_params layout tenant_url user)
    |> Lwt.return
  ;;
end

module EmailVerification = struct
  let email_params layout validation_url contact =
    global_params layout contact.Contact.user
    @ [ "verificationUrl", validation_url ]
  ;;

  let create pool message_language layout contact email_address token =
    let%lwt template, language =
      find_by_label_to_send pool message_language Label.EmailVerification
    in
    let layout = create_layout layout in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let validation_url =
      Pool_common.
        [ ( Message.Field.Language
          , language |> Language.show |> CCString.lowercase_ascii )
        ; Message.Field.Token, Email.Token.value token
        ]
      |> create_public_url_with_params url "/email-verified"
    in
    let%lwt sender = default_sender_of_pool pool in
    prepare_email
      language
      template
      sender
      email_address
      layout
      (email_params layout validation_url contact)
    |> Lwt.return
  ;;
end

module ExperimentInvitation = struct
  let email_params layout experiment public_url contact =
    let open Experiment in
    let id = experiment.Experiment.id |> Id.value in
    global_params layout contact.Contact.user
    @ [ ( "experimentUrl"
        , create_public_url public_url (Format.asprintf "experiments/%s" id) )
      ]
    @ experiment_params layout experiment
  ;;

  let prepare tenant experiment =
    let open Message_utils in
    let pool = tenant.Pool_tenant.database_label in
    let%lwt sys_langs = Settings.find_languages pool in
    let%lwt templates =
      find_all_by_label_to_send
        pool
        ~entity_uuids:[ Experiment.(Id.to_common experiment.Experiment.id) ]
        sys_langs
        Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = sender_of_experiment pool experiment in
    let layout = layout_from_tenant tenant in
    let fnc (contact : Contact.t) =
      let open CCResult in
      let message_language =
        experiment_message_language sys_langs experiment contact
      in
      let* lang, template =
        find_template_by_language templates message_language
      in
      let params = email_params layout experiment tenant_url contact in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt.return fnc
  ;;

  let create ({ Pool_tenant.database_label; _ } as tenant) experiment contact =
    let open Message_utils in
    let%lwt sys_langs = Settings.find_languages database_label in
    let message_language =
      experiment_message_language sys_langs experiment contact
    in
    let%lwt template, language =
      find_by_label_to_send
        database_label
        message_language
        Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool database_label in
    let%lwt sender = sender_of_experiment database_label experiment in
    let layout = layout_from_tenant tenant in
    let params = email_params layout experiment tenant_url contact in
    prepare_email
      language
      template
      sender
      (Contact.email_address contact)
      layout
      params
    |> Lwt.return
  ;;
end

module PasswordChange = struct
  let email_params = global_params

  let create pool message_langauge tenant user =
    let%lwt template, language =
      find_by_label_to_send pool message_langauge Label.PasswordChange
    in
    let layout = layout_from_tenant tenant in
    let email = Pool_user.user_email_address user in
    let%lwt sender = default_sender_of_pool pool in
    prepare_email
      language
      template
      sender
      email
      layout
      (email_params layout user)
    |> Lwt.return
  ;;
end

module PasswordReset = struct
  let email_params layout reset_url user =
    global_params layout user @ [ "resetUrl", reset_url ]
  ;;

  let create pool message_language layout user =
    let open Utils.Lwt_result.Infix in
    let email = Pool_user.user_email_address user in
    let%lwt template, language =
      find_by_label_to_send pool message_language Label.PasswordReset
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = default_sender_of_pool pool in
    let open Pool_common in
    let* reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:(Pool_database.to_ctx pool)
        (Pool_user.EmailAddress.value email)
      ||> function
      | None ->
        Logs.err ~src (fun m ->
          m
            ~tags:(Pool_database.Logger.Tags.create pool)
            "Reset token not found");
        Error Message.PasswordResetFailMessage
      | Some token -> Ok token
    in
    let layout = create_layout layout in
    let reset_url =
      Message.
        [ Field.Token, reset_token
        ; Field.Language, language |> Language.show |> CCString.lowercase_ascii
        ]
      |> create_public_url_with_params
           url
           (prepend_root_directory pool "/reset-password/")
    in
    prepare_email
      language
      template
      sender
      email
      layout
      (email_params layout reset_url user)
    |> Lwt_result.return
  ;;
end

module PhoneVerification = struct
  let message_params token =
    [ "token", Pool_common.VerificationCode.value token ]
  ;;

  let create_text_message
    pool
    message_language
    (tenant : Pool_tenant.t)
    cell_phone
    token
    =
    let open Text_message in
    let%lwt { sms_text; _ }, _ =
      find_by_label_to_send pool message_language Label.PhoneVerification
    in
    render_and_create
      cell_phone
      tenant.Pool_tenant.title
      (sms_text, message_params token)
    |> Lwt_result.return
  ;;
end

module ProfileUpdateTrigger = struct
  let email_params layout tenant_url contact =
    let profile_url = create_public_url tenant_url "/user/personal-details" in
    global_params layout contact.Contact.user @ [ "profileUrl", profile_url ]
  ;;

  let prepare pool tenant =
    let open Message_utils in
    let%lwt sys_langs = Settings.find_languages pool in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionReschedule
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = default_sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let fnc contact =
      let open CCResult in
      let message_langauge = contact_language sys_langs contact in
      let* lang, template =
        find_template_by_language templates message_langauge
      in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        (email_params layout url contact)
      |> CCResult.return
    in
    Lwt.return fnc
  ;;
end

module SessionCancellation = struct
  let email_params
    language
    layout
    (experiment : Experiment.t)
    session
    follow_up_sessions
    reason
    contact
    =
    let follow_up_sessions =
      follow_up_sessions, Pool_common.I18n.SessionCancellationMessageFollowUps
    in
    global_params layout contact.Contact.user
    @ [ "reason", reason |> Session.CancellationReason.value ]
    @ experiment_params layout experiment
    @ session_params ~follow_up_sessions layout language session
  ;;

  let prepare pool tenant experiment sys_langs session follow_up_sessions =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionCancellation
    in
    let%lwt sender = sender_of_experiment pool experiment in
    let layout = layout_from_tenant tenant in
    let fnc reason (contact : Contact.t) =
      let open CCResult in
      let message_language =
        experiment_message_language sys_langs experiment contact
      in
      let* lang, template =
        find_template_by_language templates message_language
      in
      let params =
        email_params
          lang
          layout
          experiment
          session
          follow_up_sessions
          reason
          contact
      in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt.return fnc
  ;;

  let prepare_text_message
    pool
    (tenant : Pool_tenant.t)
    experiment
    sys_langs
    session
    follow_up_sessions
    =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionCancellation
    in
    let title = tenant.Pool_tenant.title in
    let layout = layout_from_tenant tenant in
    let fnc reason (contact : Contact.t) cell_phone =
      let open CCResult in
      let message_language =
        experiment_message_language sys_langs experiment contact
      in
      let* lang, template =
        find_template_by_language templates message_language
      in
      let params =
        email_params
          lang
          layout
          experiment
          session
          follow_up_sessions
          reason
          contact
      in
      Text_message.render_and_create cell_phone title (template.sms_text, params)
      |> CCResult.return
    in
    Lwt.return fnc
  ;;
end

module SessionReminder = struct
  let email_params lang layout experiment session assignment =
    global_params layout assignment.Assignment.contact.Contact.user
    @ experiment_params layout experiment
    @ session_params layout lang session
    @ assignment_params assignment
  ;;

  let find_template pool experiment session language =
    find_by_label_to_send
      ~entity_uuids:
        [ Session.Id.to_common session.Session.id
        ; Experiment.Id.to_common experiment.Experiment.id
        ]
      pool
      language
      Label.SessionReminder
  ;;

  let create
    pool
    tenant
    system_languages
    experiment
    session
    ({ Assignment.contact; _ } as assignment)
    =
    let open Message_utils in
    let message_language =
      experiment_message_language system_languages experiment contact
    in
    let%lwt template, language =
      find_template pool experiment session message_language
    in
    let%lwt sender = sender_of_experiment pool experiment in
    let layout = layout_from_tenant tenant in
    let params = email_params language layout experiment session assignment in
    prepare_email
      language
      template
      sender
      (Contact.email_address contact)
      layout
      params
    |> Lwt.return
  ;;

  let prepare_emails pool tenant sys_langs experiment session =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send
        ~entity_uuids:
          [ Session.Id.to_common session.Session.id
          ; Experiment.Id.to_common experiment.Experiment.id
          ]
        pool
        sys_langs
        Label.SessionReminder
    in
    let%lwt sender = sender_of_experiment pool experiment in
    let layout = layout_from_tenant tenant in
    let fnc ({ Assignment.contact; _ } as assignment) =
      let open CCResult in
      let message_language =
        experiment_message_language sys_langs experiment contact
      in
      let* lang, template =
        find_template_by_language templates message_language
      in
      let params = email_params lang layout experiment session assignment in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt.return fnc
  ;;

  let prepare_text_messages
    pool
    (tenant : Pool_tenant.t)
    sys_langs
    experiment
    session
    =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send
        ~entity_uuids:
          [ Session.Id.to_common session.Session.id
          ; Experiment.Id.to_common experiment.Experiment.id
          ]
        pool
        sys_langs
        Label.SessionReminder
    in
    let title = tenant.Pool_tenant.title in
    let layout = layout_from_tenant tenant in
    let fnc ({ Assignment.contact; _ } as assignment) cell_phone =
      let open CCResult in
      let message_language =
        experiment_message_language sys_langs experiment contact
      in
      let* lang, template =
        find_template_by_language templates message_language
      in
      let params = email_params lang layout experiment session assignment in
      Text_message.render_and_create cell_phone title (template.sms_text, params)
      |> CCResult.return
    in
    Lwt.return fnc
  ;;
end

module SessionReschedule = struct
  let email_params lang layout experiment session new_start new_duration contact
    =
    let open Pool_common.Utils.Time in
    let open Session in
    global_params layout contact.Contact.user
    @ [ "newStart", new_start |> Start.value |> formatted_date_time
      ; "newDuration", new_duration |> Duration.value |> formatted_timespan
      ]
    @ experiment_params layout experiment
    @ session_params layout lang session
  ;;

  let prepare pool tenant experiment sys_langs session admin_contact =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionReschedule
    in
    let%lwt sender = sender_of_contact_person pool admin_contact in
    let layout = layout_from_tenant tenant in
    let fnc (contact : Contact.t) new_start new_duration =
      let open CCResult in
      let message_language =
        experiment_message_language sys_langs experiment contact
      in
      let* lang, template =
        find_template_by_language templates message_language
      in
      let params =
        email_params
          lang
          layout
          experiment
          session
          new_start
          new_duration
          contact
      in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt.return fnc
  ;;
end

module SignUpVerification = struct
  let email_params layout verification_url firstname lastname =
    let open Pool_user in
    let firstname = firstname |> Firstname.value in
    let lastname = lastname |> Lastname.value in
    [ "name", Format.asprintf "%s %s" firstname lastname
    ; "firstname", firstname
    ; "lastname", lastname
    ; "verificationUrl", verification_url
    ; "siteTitle", layout.site_title
    ; "siteUrl", layout.link
    ]
  ;;

  let create
    pool
    preferred_language
    tenant
    email_address
    token
    firstname
    lastname
    =
    let%lwt template, language =
      find_by_label_to_send pool preferred_language Label.SignUpVerification
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = default_sender_of_pool pool in
    let verification_url =
      Pool_common.
        [ ( Message.Field.Language
          , language |> Language.show |> CCString.lowercase_ascii )
        ; Message.Field.Token, Email.Token.value token
        ]
      |> create_public_url_with_params url "/email-verified"
    in
    let layout = layout_from_tenant tenant in
    prepare_email
      language
      template
      sender
      email_address
      layout
      (email_params layout verification_url firstname lastname)
    |> Lwt.return
  ;;
end

module UserImport = struct
  let email_address = function
    | `Admin admin -> Admin.email_address admin
    | `Contact contact -> Contact.email_address contact
  ;;

  let language default_language = function
    | `Admin _ -> Pool_common.Language.En
    | `Contact (contact : Contact.t) ->
      contact.Contact.language |> CCOption.value ~default:default_language
  ;;

  let email_params layout confirmation_url user =
    let user =
      match user with
      | `Admin admin -> Admin.user admin
      | `Contact contact -> Contact.user contact
    in
    global_params layout user @ [ "confirmationUrl", confirmation_url ]
  ;;

  let prepare pool tenant =
    let languages = Pool_common.Language.all in
    let templates = Hashtbl.create (CCList.length languages) in
    let%lwt () =
      find_all_by_label_to_send pool Pool_common.Language.all Label.UserImport
      |> Lwt.map
           (CCList.iter (fun ({ Entity.language; _ } as t) ->
              Hashtbl.add templates language t))
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt default_language = Settings.default_language pool in
    let%lwt sender = default_sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    Lwt.return
    @@ fun (user : [< `Admin of Admin.t | `Contact of Contact.t ]) token ->
    let language = language default_language user in
    let confirmation_url =
      Pool_common.
        [ ( Message.Field.Language
          , language |> Language.show |> CCString.lowercase_ascii )
        ; Message.Field.Token, token
        ]
      |> create_public_url_with_params url "/import-confirmation"
    in
    let template = Hashtbl.find templates language in
    prepare_email
      language
      template
      sender
      (email_address user)
      layout
      (email_params layout confirmation_url user)
  ;;
end

module WaitingListConfirmation = struct
  let base_params layout contact = contact.Contact.user |> global_params layout

  let email_params layout contact experiment =
    base_params layout contact @ public_experiment_params layout experiment
  ;;

  let create ({ Pool_tenant.database_label; _ } as tenant) contact experiment =
    let open Utils.Lwt_result.Infix in
    let open Message_utils in
    let%lwt system_languages = Settings.find_languages database_label in
    let message_language =
      public_experiment_message_language system_languages experiment contact
    in
    let* sender = sender_of_public_experiment database_label experiment in
    let layout = layout_from_tenant tenant in
    let%lwt template, language =
      find_by_label_to_send
        ~entity_uuids:Experiment.[ experiment.Public.id |> Id.to_common ]
        database_label
        message_language
        Label.WaitingListConfirmation
    in
    let email = contact |> Contact.email_address in
    let params = email_params layout contact experiment in
    prepare_email language template sender email layout params
    |> Lwt_result.return
  ;;
end
