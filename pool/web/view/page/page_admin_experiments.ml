open Tyxml.Html
open Component
open Input
open CCFun
module HttpUtils = Http_utils
module Message = Pool_common.Message
module Field = Message.Field

let build_experiment_path experiment =
  Format.asprintf "/admin/experiments/%s/%s" Experiment.(Id.value experiment.id)
;;

let notifications
  ?(can_update_experiment = false)
  language
  sys_languages
  (experiment : Experiment.t)
  message_templates
  =
  let open CCList in
  let open Pool_common in
  let open Message_template in
  match experiment.Experiment.language with
  | Some _ -> txt ""
  | None ->
    message_templates
    |> filter_map (fun (label, templates) ->
      if is_empty templates || not can_update_experiment
      then None
      else
        filter
          (fun lang ->
            find_opt
              (fun { language; _ } -> Language.equal language lang)
              templates
            |> CCOption.is_none)
          sys_languages
        |> function
        | [] -> None
        | langs -> Some (label, langs))
    |> (function
     | [] -> txt ""
     | missing_templates ->
       let list =
         missing_templates
         |> CCList.map (fun (label, languages) ->
           Format.asprintf
             "%s: [%s]"
             (Label.to_human label)
             (CCString.concat
                ", "
                (CCList.map Pool_common.Language.show languages))
           |> txt
           |> CCList.pure
           |> li)
         |> ul
       in
       [ p
           [ txt
               Pool_common.(
                 Utils.hint_to_string language I18n.MissingMessageTemplates)
           ]
       ; list
       ]
       |> Notification.notification language `Warning)
;;

let message_template_buttons
  can_update_experiment
  sys_languages
  (experiment : Experiment.t)
  message_templates
  =
  let open Message_template in
  let build_button label =
    build_experiment_path experiment Label.(prefixed_human_url label)
    |> Button.add label
  in
  let exclude =
    experiment.Experiment.language
    |> CCOption.map (fun experiment_language ->
      CCList.filter
        (Pool_common.Language.equal experiment_language %> not)
        sys_languages)
  in
  message_templates
  |> CCList.filter_map (fun (label, templates) ->
    if CCList.is_empty (filter_languages ?exclude sys_languages templates)
    then None
    else label |> build_button |> CCOption.pure)
  |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
  |> fun buttons -> if can_update_experiment then Some buttons else None
;;

let message_templates_html
  ?(can_update_experiment = false)
  language
  csrf
  experiment
  sys_languages
  message_templates
  =
  let open Message_template in
  let experiment_path = build_experiment_path experiment in
  let buttons =
    message_template_buttons
      can_update_experiment
      sys_languages
      experiment
      message_templates
  in
  let build_path append = prefixed_template_url ~append %> experiment_path in
  let edit_path = build_path "edit" in
  let delete_path = build_path "delete", csrf in
  Page_admin_message_template.table
    ?buttons
    ~can_update_experiment
    ~delete_path
    language
    (CCList.flat_map (fun (_, templates) -> templates) message_templates)
    edit_path
;;

let list Pool_context.{ language; _ } experiments query =
  let url = Uri.of_string "/admin/experiments" in
  let data_table =
    Component.DataTable.create_meta
      ?filter:Experiment.filterable_by
      ~search:Experiment.searchable_by
      url
      query
      language
  in
  let cols =
    let create_experiment : [ | Html_types.flow5 ] elt =
      Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Message.(Add (Some Field.Experiment)))
        "/admin/experiments/create"
    in
    [ `column Experiment.column_title
    ; `column Experiment.column_public_title
    ; `custom create_experiment
    ]
  in
  let th_class = [ "w-6"; "w-4"; "w-2" ] in
  let row (experiment : Experiment.t) =
    let open Experiment in
    [ txt (Title.value experiment.title)
    ; txt (PublicTitle.value experiment.public_title)
    ; Format.asprintf
        "/admin/experiments/%s"
        (Experiment.Id.value experiment.id)
      |> Input.link_as_button ~icon:Icon.Eye
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  DataTable.make
    ~target_id:"experiment-list"
    ~th_class
    ~cols
    ~row
    data_table
    experiments
;;

let index (Pool_context.{ language; _ } as context) experiments query =
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.ExperimentListTitle) ]
    ; list context experiments query
    ]
;;

let experiment_form
  ?experiment
  Pool_context.{ language; csrf; _ }
  contact_persons
  organisational_units
  smtp_auth_list
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  flash_fetcher
  =
  let open Pool_common in
  let open Experiment in
  let action =
    match experiment with
    | None -> "/admin/experiments"
    | Some experiment ->
      Format.asprintf
        "/admin/experiments/%s"
        (experiment.id |> Experiment.Id.value)
  in
  let checkbox_element ?hints ?(default = false) field fnc =
    checkbox_element
      language
      ?hints
      field
      ~value:(experiment |> CCOption.map_or ~default fnc)
      ~flash_fetcher
  in
  let value = flip (CCOption.map_or ~default:"") experiment in
  let experiment_type_select =
    let open ExperimentType in
    selector
      language
      Field.ExperimentType
      show
      all
      (CCOption.bind experiment (fun (e : Experiment.t) -> e.experiment_type))
      ~add_empty:true
      ~flash_fetcher
      ()
  in
  let language_select =
    let open Language in
    selector
      ~add_empty:true
      ~hints:[ I18n.ExperimentLanguage ]
      language
      Message.Field.Language
      show
      all
      (CCOption.bind experiment (fun { language; _ } -> language))
      ()
  in
  let lead_time_group field get_value value default_value =
    div
      [ timespan_picker
          language
          field
          ~hints:
            [ I18n.TimeSpanPickerHint
            ; I18n.DefaultReminderLeadTime (default_value |> value)
            ]
          ?value:CCOption.(bind experiment get_value)
          ~flash_fetcher
      ]
  in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack" ]
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ div
            ~a:[ a_class [ "grid-col-2" ] ]
            [ input_element
                language
                `Text
                Field.Title
                ~value:(value title_value)
                ~required:true
                ~flash_fetcher
            ; input_element
                language
                `Text
                Field.PublicTitle
                ~value:(value public_title_value)
                ~required:(CCOption.is_some experiment)
                ~flash_fetcher
            ; textarea_element
                language
                Field.InternalDescription
                ?value:
                  (CCOption.bind experiment (fun { internal_description; _ } ->
                     internal_description
                     |> CCOption.map InternalDescription.value))
                ~flash_fetcher
            ; textarea_element
                language
                Field.PublicDescription
                ?value:
                  (CCOption.bind experiment (fun { public_description; _ } ->
                     public_description |> CCOption.map PublicDescription.value))
                ~flash_fetcher
            ; language_select
            ; experiment_type_select
            ; input_element
                language
                `Text
                Field.CostCenter
                ?value:
                  (CCOption.bind experiment (fun e ->
                     e.cost_center |> CCOption.map CostCenter.value))
                ~flash_fetcher
            ; organisational_units_selector
                language
                organisational_units
                (CCOption.bind experiment (fun ex -> ex.organisational_unit))
            ]
        ; div
            [ h3
                ~a:[ a_class [ "heading-3" ] ]
                [ txt
                    (Utils.text_to_string
                       language
                       I18n.ExperimentMessagingSubtitle)
                ]
            ; div
                ~a:[ a_class [ "grid-col-2" ] ]
                [ admin_select
                    language
                    contact_persons
                    (CCOption.bind experiment (fun exp -> exp.contact_person_id))
                    Field.ContactPerson
                    ~hints:[ I18n.ExperimentContactPerson ]
                    ()
                ; selector
                    language
                    Field.Smtp
                    Email.SmtpAuth.(fun ({ id; _ } : t) -> Id.value id)
                    smtp_auth_list
                    CCOption.(
                      experiment
                      >>= fun { smtp_auth_id; _ } ->
                      smtp_auth_id
                      >>= Email.SmtpAuth.(
                            fun smtp_auth_id ->
                              CCList.find_opt
                                (fun ({ id; _ } : t) ->
                                  Id.equal id smtp_auth_id)
                                smtp_auth_list))
                    ~option_formatter:
                      Email.SmtpAuth.(fun { label; _ } -> Label.value label)
                    ~flash_fetcher
                    ~add_empty:true
                    ()
                ]
            ]
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ checkbox_element
                ~hints:[ I18n.DirectRegistrationDisbled ]
                Field.DirectRegistrationDisabled
                direct_registration_disabled_value
            ; checkbox_element
                ~hints:[ I18n.RegistrationDisabled ]
                Field.RegistrationDisabled
                registration_disabled_value
            ; checkbox_element
                ~hints:[ I18n.AllowUninvitedSignup ]
                Field.AllowUninvitedSignup
                allow_uninvited_signup_value
            ; checkbox_element
                ~hints:[ I18n.ExternalDataRequired ]
                Field.ExternalDataRequired
                external_data_required_value
            ; checkbox_element
                Field.ShowExteralDataIdLinks
                show_external_data_id_links_value
            ]
        ; div
            [ h3
                ~a:[ a_class [ "heading-3" ] ]
                [ txt (Utils.text_to_string language I18n.SessionReminder) ]
            ; div
                ~a:[ a_class [ "stack" ] ]
                [ p
                    [ txt
                        (Utils.text_to_string
                           language
                           I18n.ExperimentSessionReminderHint)
                    ]
                ; div
                    ~a:[ a_class [ "grid-col-2" ] ]
                    [ lead_time_group
                        Field.EmailLeadTime
                        email_session_reminder_lead_time_value
                        Reminder.EmailLeadTime.value
                        default_email_reminder_lead_time
                    ; lead_time_group
                        Field.TextMessageLeadTime
                        text_message_session_reminder_lead_time_value
                        Reminder.TextMessageLeadTime.value
                        default_text_msg_reminder_lead_time
                    ]
                ]
            ]
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ div
            ~a:[ a_class [ "push"; "flexrow"; "flex-gap-lg" ] ]
            [ reset_form_button language
            ; submit_element
                language
                Message.(
                  let field = Some Field.Experiment in
                  match experiment with
                  | None -> Create field
                  | Some _ -> Update field)
                ~submit_type:`Primary
                ()
            ]
        ]
    ]
;;

let create
  (Pool_context.{ language; _ } as context)
  organisational_units
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  contact_persons
  smtp_auth_list
  flash_fetcher
  =
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack" ] ]
    [ h1
        [ txt
            (Utils.control_to_string
               language
               Message.(Create (Some Field.Experiment)))
        ]
    ; experiment_form
        context
        contact_persons
        organisational_units
        smtp_auth_list
        default_email_reminder_lead_time
        default_text_msg_reminder_lead_time
        flash_fetcher
    ]
;;

let edit
  ?(allowed_to_assign = false)
  experiment
  ({ Pool_context.language; csrf; query_language; _ } as context)
  default_email_reminder_lead_time
  default_text_msg_reminder_lead_time
  contact_persons
  organisational_units
  smtp_auth_list
  (available_tags, current_tags)
  (available_participation_tags, current_participation_tags)
  flash_fetcher
  =
  let form =
    experiment_form
      ~experiment
      context
      contact_persons
      organisational_units
      smtp_auth_list
      default_email_reminder_lead_time
      default_text_msg_reminder_lead_time
      flash_fetcher
  in
  let tags_html (available, current) field =
    if allowed_to_assign
    then (
      let remove_action tag =
        Format.asprintf
          "%s/%s/remove"
          Field.(field |> human_url)
          Tags.(Id.value tag.Tags.id)
        |> build_experiment_path experiment
      in
      let assign_action =
        Http_utils.externalize_path_with_lang
          query_language
          (Format.asprintf "%s/assign" Field.(field |> human_url)
           |> build_experiment_path experiment)
      in
      div
        ~a:[ a_class [ "switcher-lg"; "flex-gap" ] ]
        [ Tag.add_tags_form context ~existing:current available assign_action
        ; Component.Tag.tag_form
            ~label:Pool_common.I18n.SelectedTags
            language
            (remove_action, csrf)
            current
        ])
    else txt ""
  in
  let tags =
    let open Pool_common in
    div
      ~a:[ a_class [ "stack" ] ]
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ Utils.nav_link_to_string language I18n.Tags |> txt ]
      ; tags_html (available_tags, current_tags) Field.Tag
      ; div
          [ h3
              ~a:[ a_class [ "heading-3" ] ]
              [ Utils.field_to_string language Field.ParticipationTag
                |> String.capitalize_ascii
                |> txt
              ]
          ; p
              [ Utils.hint_to_string language I18n.ParticipationTagsHint |> txt
              ]
          ; tags_html
              (available_participation_tags, current_participation_tags)
              Field.ParticipationTag
          ]
      ]
  in
  [ div ~a:[ a_class [ "stack-lg" ] ] [ form; tags ] ]
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Edit (Some Field.Experiment)))
         experiment)
;;

let detail
  ({ Experiment.id; _ } as experiment)
  session_count
  message_templates
  sys_languages
  contact_person
  smtp_account
  tags
  participation_tags
  ({ Pool_context.language; csrf; guardian; _ } as context)
  =
  let open Pool_common in
  let can_update_experiment =
    Guard.PermissionOnTarget.validate
      (Experiment.Guard.Access.update_permission_on_target id)
      guardian
  in
  let notifications =
    notifications
      ~can_update_experiment
      language
      sys_languages
      experiment
      message_templates
  in
  let delete_form =
    match session_count > 0 with
    | true ->
      div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
        [ submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~submit_type:`Disabled
            ~has_icon:Icon.TrashOutline
            ~classnames:[ "small" ]
            ()
        ; div
            ~a:[ a_class [ "grow" ] ]
            [ txt
                (Message.ExperimentSessionCountNotZero
                 |> Utils.error_to_string language)
            ]
        ]
    | false ->
      div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
        [ form
            ~a:
              [ a_method `Post
              ; a_action
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/admin/experiments/%s/delete"
                        (experiment.Experiment.id |> Experiment.Id.value)))
              ; a_user_data
                  "confirmable"
                  (Utils.confirmable_to_string language I18n.DeleteExperiment)
              ]
            [ csrf_element csrf ()
            ; submit_element
                language
                Message.(Delete (Some Field.Experiment))
                ~classnames:[ "small" ]
                ~submit_type:`Error
                ~has_icon:Icon.TrashOutline
                ()
            ]
        ]
  in
  let reset_invitation_form =
    let open Experiment in
    let last_reset_at =
      match experiment.invitation_reset_at with
      | None -> txt ""
      | Some reset_at ->
        span
          [ Utils.hint_to_string
              language
              (I18n.ResetInvitationsLastReset (InvitationResetAt.value reset_at))
            |> Unsafe.data
          ]
    in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
      [ form
          ~a:
            [ a_method `Post
            ; a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/experiments/%s/reset-invitations"
                      (experiment.Experiment.id |> Experiment.Id.value)))
            ; a_user_data
                "confirmable"
                (Utils.confirmable_to_string language I18n.ResetInvitations)
            ]
          [ csrf_element csrf ()
          ; submit_element
              language
              Message.(Reset (Some Field.Invitations))
              ~classnames:[ "small" ]
              ~submit_type:`Primary
              ~has_icon:Icon.RefreshOutline
              ()
          ]
      ; div
          ~a:[ a_class [ "grow"; "flexcolumn" ] ]
          [ span [ txt (Utils.hint_to_string language I18n.ResetInvitations) ]
          ; last_reset_at
          ]
      ]
  in
  let setting =
    if can_update_experiment
    then
      [ div
          ~a:[ a_class [ "stack-md" ] ]
          [ h2
              ~a:[ a_class [ "heading-2" ] ]
              [ txt
                  (Utils.field_to_string language Message.Field.Settings
                   |> CCString.capitalize_ascii)
              ]
          ; reset_invitation_form
          ; delete_form
          ]
      ]
    else []
  in
  let bool_to_string = Utils.bool_to_string language in
  let open Experiment in
  let vertical_table =
    Table.vertical_table
      ~classnames:[ "layout-fixed" ]
      ~align_top:true
      `Striped
      language
  in
  let html =
    let experiment_table =
      let boolean_value fnc = fnc experiment |> bool_to_string |> txt in
      let default = "" in
      Message.
        [ Field.PublicTitle, experiment.public_title |> PublicTitle.value |> txt
        ; ( Field.ExperimentType
          , experiment.experiment_type
            |> CCOption.map_or ~default:"" ExperimentType.show
            |> txt )
        ; ( Field.InternalDescription
          , experiment.internal_description
            |> CCOption.map_or ~default:(txt "") (fun desc ->
              desc |> InternalDescription.value |> HttpUtils.add_line_breaks) )
        ; ( Field.PublicDescription
          , experiment.public_description
            |> CCOption.map_or ~default:(txt "") (fun desc ->
              desc |> PublicDescription.value |> HttpUtils.add_line_breaks) )
        ; ( Field.Language
          , experiment.language
            |> CCOption.map_or ~default (fun lang -> lang |> Language.show)
            |> txt )
        ; ( Field.CostCenter
          , experiment.cost_center
            |> CCOption.map_or ~default CostCenter.value
            |> txt )
        ; ( Field.OrganisationalUnit
          , experiment.organisational_unit
            |> CCOption.map_or
                 ~default
                 Organisational_unit.(fun ou -> ou.name |> Name.value)
            |> txt )
        ; ( Field.ContactPerson
          , contact_person |> CCOption.map_or ~default Admin.full_name |> txt )
        ; ( Field.Smtp
          , smtp_account
            |> CCOption.map_or
                 ~default
                 Email.SmtpAuth.(fun auth -> auth.label |> Label.value)
            |> txt )
        ; ( Field.DirectRegistrationDisabled
          , direct_registration_disabled_value |> boolean_value )
        ; ( Field.RegistrationDisabled
          , registration_disabled_value |> boolean_value )
        ; ( Field.AllowUninvitedSignup
          , allow_uninvited_signup_value |> boolean_value )
        ; ( Field.ExternalDataRequired
          , external_data_required_value |> boolean_value )
        ; ( Field.ShowExteralDataIdLinks
          , show_external_data_id_links_value |> boolean_value )
        ; ( Field.ExperimentEmailReminderLeadTime
          , email_session_reminder_lead_time_value experiment
            |> CCOption.map_or ~default:"-" Utils.Time.formatted_timespan
            |> txt )
        ; ( Field.ExperimentTextMessageReminderLeadTime
          , text_message_session_reminder_lead_time_value experiment
            |> CCOption.map_or ~default:"-" Utils.Time.formatted_timespan
            |> txt )
        ; ( Field.InvitationResetAt
          , experiment.invitation_reset_at
            |> CCOption.map_or ~default:"-" InvitationResetAt.to_human
            |> txt )
        ]
      |> vertical_table
    in
    let message_template =
      div
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt (Utils.nav_link_to_string language I18n.MessageTemplates) ]
        ; Page_admin_message_template.(
            experiment_help
              ~entity:(Experiment experiment.id)
              language
              (message_templates |> CCList.map fst))
        ; div
            ~a:[ a_class [ "gap" ] ]
            [ message_templates_html
                ~can_update_experiment
                language
                csrf
                experiment
                sys_languages
                message_templates
            ]
        ]
    in
    let tag_overview =
      let build (title, tags) =
        div
          [ h3
              ~a:[ a_class [ "heading-3" ] ]
              [ Utils.nav_link_to_string language title |> txt ]
          ; Component.Tag.tag_list language tags
          ]
      in
      I18n.[ Tags, tags; ParticipationTags, participation_tags ]
      |> CCList.map build
      |> div ~a:[ a_class [ "switcher"; "flex-gap" ] ]
    in
    [ div
        ~a:[ a_class [ "stack-lg" ] ]
        ([ notifications; experiment_table; tag_overview; message_template ]
         @ setting)
    ]
  in
  let edit_button =
    if can_update_experiment
    then
      link_as_button
        ~icon:Icon.Create
        ~classnames:[ "small" ]
        ~control:(language, Message.(Edit (Some Field.Experiment)))
        (Format.asprintf
           "/admin/experiments/%s/edit"
           (experiment.id |> Experiment.Id.value))
      |> CCOption.some
    else None
  in
  Layout.Experiment.(
    create
      ~active_navigation:I18n.Overview
      ?buttons:edit_button
      context
      (NavLink I18n.Overview)
      experiment
      html)
;;

let invitations
  experiment
  key_list
  template_list
  query_experiments
  query_tags
  matching_filter_count
  invitation_count
  filtered_contacts
  ({ Pool_context.language; _ } as context)
  =
  let open Pool_common in
  [ div
      ~a:[ a_class [ "stack" ] ]
      [ p
          [ a
              ~a:
                [ a_href
                    (experiment.Experiment.id
                     |> Experiment.Id.value
                     |> Format.asprintf "admin/experiments/%s/invitations/sent"
                     |> Sihl.Web.externalize_path)
                ]
              [ txt (Utils.text_to_string language I18n.SentInvitations) ]
          ]
      ; Page_admin_invitations.Partials.send_invitation
          context
          experiment
          key_list
          template_list
          query_experiments
          query_tags
          filtered_contacts
          matching_filter_count
          invitation_count
      ]
  ]
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Invitations
         context
         (NavLink I18n.Invitations)
         experiment)
;;

let users ?hint role experiment applicable_admins currently_assigned context =
  let base_url field admin =
    Format.asprintf
      "/admin/experiments/%s/%s/%s"
      Experiment.(experiment.id |> Id.value)
      (Field.show field)
      (Admin.id admin |> Admin.Id.value)
    |> Sihl.Web.externalize_path
  in
  let field =
    let open Message in
    match role with
    | `Assistants -> Field.Assistants
    | `Experimenter -> Field.Experimenter
  in
  Page_admin_experiment_users.role_assignment
    ?hint
    (base_url field)
    field
    context
    ~assign:"assign"
    ~unassign:"unassign"
    ~applicable:applicable_admins
    ~current:currently_assigned
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:(Pool_common.I18n.Field field)
         context
         (NavLink (Pool_common.I18n.Field field))
         experiment)
;;

let message_template_form
  ({ Pool_context.language; _ } as context)
  tenant
  experiment
  languages
  label
  form_context
  flash_fetcher
  =
  let open Message_template in
  let open Pool_common in
  let control_to_title control =
    Layout.Experiment.Text
      (Format.asprintf
         "%s %s"
         (control |> Utils.control_to_string language)
         (label |> Label.to_human |> CCString.lowercase_ascii))
  in
  let control =
    match form_context with
    | `Create _ -> Message.(Create None)
    | `Update _ -> Message.(Edit None)
  in
  let action =
    let path =
      Format.asprintf
        "/admin/experiments/%s/%s"
        Experiment.(Id.value experiment.Experiment.id)
    in
    match form_context with
    | `Create t -> path (Label.prefixed_human_url t.label)
    | `Update t -> path (prefixed_template_url t)
  in
  let text_elements =
    Component.MessageTextElements.message_template_help
      ~experiment
      language
      tenant
      label
  in
  let open Page_admin_message_template in
  template_form
    context
    ~entity:(Experiment experiment.Experiment.id)
    ?languages
    ~text_elements
    ?fixed_language:experiment.Experiment.language
    form_context
    action
    flash_fetcher
  |> CCList.return
  |> Layout.Experiment.create context (control_to_title control) experiment
;;
