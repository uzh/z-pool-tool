open Tyxml.Html
open Component
open Input
open CCFun
module HttpUtils = Http_utils
module Message = Pool_common.Message
module Field = Message.Field

let build_experiment_path ?suffix experiment =
  let base =
    Format.asprintf "/admin/experiments/%s" Experiment.(Id.value experiment.id)
  in
  suffix |> CCOption.map_or ~default:base (Format.asprintf "%s/%s" base)
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

module Statistics = struct
  open Experiment.Statistics

  let make language statistics =
    let int_to_txt i = i |> CCInt.to_string |> txt in
    let with_tooltip html tooltip =
      div
        ~a:[ a_class [ "has-icon flex-gap-sm" ] ]
        [ span [ html ]
        ; div
            ~a:[ a_class [ "tooltip-wrapper" ] ]
            [ Icon.(to_html HelpOutline)
            ; p ~a:[ a_class [ "tooltip" ] ] [ tooltip ]
            ]
        ]
    in
    let to_table =
      Component.Table.vertical_table ~th_class:[ "w-8" ] `Simple language
    in
    let registration_possible_html =
      let open RegistrationPossible in
      let html =
        statistics
        |> registration_possible
        |> value
        |> Pool_common.Utils.bool_to_string language
        |> txt
      in
      let tooltip = hint |> Pool_common.Utils.hint_to_string language |> txt in
      with_tooltip html tooltip
    in
    let sending_invitations_html =
      let open SendingInvitations in
      let html =
        statistics
        |> sending_invitations
        |> show
        |> CCString.capitalize_ascii
        |> txt
      in
      let tooltip =
        hint
        |> Pool_common.Utils.hint_to_string language
        |> HttpUtils.add_line_breaks
      in
      with_tooltip html tooltip
    in
    let experiment_statistics =
      [ RegistrationPossible.field, registration_possible_html
      ; SendingInvitations.field, sending_invitations_html
      ; SessionCount.(field, statistics |> session_count |> value |> int_to_txt)
      ; SentInvitationCount.(
          field, statistics |> sent_invitation_count |> value |> int_to_txt)
      ]
    in
    let assignments_statistics =
      [ ShowUpCount.(field, statistics |> showup_count |> value |> int_to_txt)
      ; NoShowCount.(field, statistics |> noshow_count |> value |> int_to_txt)
      ; ParticipationCount.(
          field, statistics |> participation_count |> value |> int_to_txt)
      ]
    in
    div
      [ h3
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.ExperimentStatistics)
          ]
      ; experiment_statistics |> to_table
      ; h4
          [ txt Pool_common.(Utils.nav_link_to_string language I18n.Assignments)
          ]
      ; assignments_statistics |> to_table
      ]
  ;;
end

let message_template_buttons
  sys_languages
  (experiment : Experiment.t)
  message_templates
  =
  let open Message_template in
  let build_button label =
    build_experiment_path ~suffix:Label.(prefixed_human_url label) experiment
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
  |> fun buttons ->
  div
    ~a:[ a_class [ "flexrow" ] ]
    [ Component.ButtonGroup.dropdown
        ~icon:Icon.Add
        ~icon_style:[ "small"; "success" ]
        ~classnames:[ "push" ]
        buttons
    ]
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
  let experiment_path suffix = build_experiment_path ~suffix experiment in
  let buttons =
    if can_update_experiment
    then message_template_buttons sys_languages experiment message_templates
    else txt ""
  in
  let build_path append template =
    experiment_path (prefixed_template_url ~append template)
  in
  let edit_path = build_path "edit" in
  let delete_path = build_path "delete", csrf in
  Page_admin_message_template.table
    ~buttons
    ~can_update_experiment
    ~delete_path
    language
    (CCList.flat_map (fun (_, templates) -> templates) message_templates)
    edit_path
;;

let list page Pool_context.{ language; _ } experiments query =
  let url =
    match page with
    | `All -> Uri.of_string "/admin/experiments"
    | `Participated (contact, _) ->
      contact
      |> Contact.id
      |> Contact.Id.value
      |> Format.asprintf "/admin/contacts/%s/past-experiments"
      |> Uri.of_string
  in
  let data_table =
    let push_url =
      match page with
      | `All -> true
      | `Participated _ -> false
    in
    Component.DataTable.create_meta
      ?filter:Experiment.filterable_by
      ~push_url
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
    let detail_btn =
      Format.asprintf "/admin/experiments/%s" (Id.value experiment.id)
      |> Input.link_as_button ~icon:Icon.Eye
    in
    let session_modal_btn contact target_id =
      let url =
        Format.asprintf
          "/admin/experiments/%s/contact-history/%s"
          (Id.value experiment.id)
          Contact.(contact |> id |> Id.value)
        |> Sihl.Web.externalize_path
      in
      let attributes =
        Htmx.
          [ hx_get url
          ; hx_swap "outerHTML"
          ; hx_target ("#" ^ target_id)
          ; a_class [ "primary" ]
          ]
      in
      button ~a:attributes [ Icon.(to_html InformationOutline) ]
    in
    let buttons =
      div ~a:[ a_class [ "flexrow"; "flex-gap-sm"; "justify-end" ] ]
      @@
      match page with
      | `All -> [ detail_btn ]
      | `Participated (contact, target_id) ->
        [ session_modal_btn contact target_id; detail_btn ]
    in
    [ txt (Title.value experiment.title)
    ; txt (PublicTitle.value experiment.public_title)
    ; buttons
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
  let experiment_list = list `All context experiments query in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.ExperimentListTitle) ]
    ; experiment_list
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
  text_messages_enabled
  flash_fetcher
  =
  let open Pool_common in
  let context_language = language in
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
      context_language
      ?hints
      field
      ~value:(experiment |> CCOption.map_or ~default fnc)
      ~flash_fetcher
  in
  let value = flip (CCOption.map_or ~default:"") experiment in
  let experiment_type_select =
    let open ExperimentType in
    selector
      context_language
      Field.ExperimentType
      show
      all
      (CCOption.bind experiment experiment_type)
      ~add_empty:true
      ~flash_fetcher
      ()
  in
  let language_select =
    let open Language in
    selector
      ~add_empty:true
      ~hints:[ I18n.ExperimentLanguage ]
      context_language
      Message.Field.Language
      show
      all
      (CCOption.bind experiment language)
      ()
  in
  let lead_time_group field get_value value default_value hint =
    let hint =
      hint
      |> CCOption.map_or
           ~default:(txt "")
           Pool_common.(
             fun hint ->
               hint
               |> Utils.hint_to_string context_language
               |> txt
               |> CCList.return
               |> Component.Notification.notification context_language `Warning)
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ hint
      ; timespan_picker
          context_language
          field
          ~hints:[ I18n.DefaultReminderLeadTime (default_value |> value) ]
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
                context_language
                `Text
                Field.Title
                ~value:(value title_value)
                ~required:true
                ~flash_fetcher
            ; input_element
                context_language
                `Text
                Field.PublicTitle
                ~value:(value public_title_value)
                ~required:(CCOption.is_some experiment)
                ~flash_fetcher
            ; textarea_element
                context_language
                Field.InternalDescription
                ?value:
                  (CCOption.bind
                     experiment
                     (internal_description
                      %> CCOption.map InternalDescription.value))
                ~flash_fetcher
            ; textarea_element
                context_language
                Field.PublicDescription
                ?value:
                  (CCOption.bind
                     experiment
                     (public_description %> CCOption.map PublicDescription.value))
                ~flash_fetcher
            ; language_select
            ; experiment_type_select
            ; input_element
                context_language
                `Text
                Field.CostCenter
                ?value:
                  (CCOption.bind
                     experiment
                     (cost_center %> CCOption.map CostCenter.value))
                ~flash_fetcher
            ; organisational_units_selector
                context_language
                organisational_units
                (CCOption.bind experiment organisational_unit)
            ]
        ; div
            [ h3
                ~a:[ a_class [ "heading-3" ] ]
                [ txt
                    (Utils.text_to_string
                       context_language
                       I18n.ExperimentMessagingSubtitle)
                ]
            ; div
                ~a:[ a_class [ "grid-col-2" ] ]
                [ admin_select
                    context_language
                    contact_persons
                    (CCOption.bind experiment contact_person_id)
                    Field.ContactPerson
                    ~hints:[ I18n.ExperimentContactPerson ]
                    ()
                ; (let open Email.SmtpAuth in
                   selector
                     context_language
                     Field.Smtp
                     (id %> Id.value)
                     smtp_auth_list
                     CCOption.(
                       experiment
                       >>= smtp_auth_id
                       >>= fun smtp_id ->
                       CCList.find_opt (id %> Id.equal smtp_id) smtp_auth_list)
                     ~option_formatter:(fun { label; _ } -> Label.value label)
                     ~flash_fetcher
                     ~add_empty:true
                     ())
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
                [ txt
                    (Utils.text_to_string context_language I18n.SessionReminder)
                ]
            ; div
                ~a:[ a_class [ "stack" ] ]
                [ p
                    [ txt
                        (Utils.text_to_string
                           context_language
                           I18n.ExperimentSessionReminderHint)
                    ]
                ; div
                    ~a:[ a_class [ "grid-col-2" ] ]
                    [ lead_time_group
                        Field.EmailLeadTime
                        email_session_reminder_lead_time_value
                        Reminder.EmailLeadTime.value
                        default_email_reminder_lead_time
                        None
                    ; lead_time_group
                        Field.TextMessageLeadTime
                        text_message_session_reminder_lead_time_value
                        Reminder.TextMessageLeadTime.value
                        default_text_msg_reminder_lead_time
                        (if text_messages_enabled
                         then None
                         else Some I18n.GtxKeyMissing)
                    ]
                ]
            ]
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ div
            ~a:[ a_class [ "push"; "flexrow"; "flex-gap-lg" ] ]
            [ reset_form_button context_language
            ; submit_element
                context_language
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
  text_messages_enabled
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
        text_messages_enabled
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
  text_messages_enabled
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
      text_messages_enabled
      flash_fetcher
  in
  let tags_html (available, current) field =
    if allowed_to_assign
    then (
      let remove_action tag =
        let suffix =
          Format.asprintf
            "%s/%s/remove"
            Field.(field |> human_url)
            Tags.(Id.value tag.Tags.id)
        in
        build_experiment_path ~suffix experiment
      in
      let assign_action =
        let path =
          build_experiment_path
            ~suffix:(Format.asprintf "%s/assign" Field.(field |> human_url))
            experiment
        in
        Http_utils.externalize_path_with_lang query_language path
      in
      div
        ~a:[ a_class [ "grid-col-2"; "flex-gap" ] ]
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
  experiment
  session_count
  message_templates
  sys_languages
  contact_person
  smtp_account
  tags
  participation_tags
  statistics
  ({ Pool_context.language; csrf; guardian; _ } as context)
  =
  let open Pool_common in
  let experiment_id = Experiment.id experiment in
  let can_update_experiment =
    Guard.PermissionOnTarget.validate
      (Experiment.Guard.Access.update_permission_on_target experiment_id)
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
                  (build_experiment_path ~suffix:"delete" experiment
                   |> Sihl.Web.externalize_path)
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
    let last_reset_at =
      match experiment |> Experiment.invitation_reset_at with
      | None -> txt ""
      | Some reset_at ->
        span
          [ Utils.hint_to_string
              language
              (I18n.ResetInvitationsLastReset
                 (Experiment.InvitationResetAt.value reset_at))
            |> Unsafe.data
          ]
    in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
      [ form
          ~a:
            [ a_method `Post
            ; a_action
                (build_experiment_path ~suffix:"reset-invitations" experiment
                 |> Sihl.Web.externalize_path)
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
  let vertical_table =
    Table.vertical_table
      ~align_top:true
      ~break_mobile:true
      ~th_class:[ "w-4" ]
      `Striped
      language
  in
  let html =
    let experiment_table =
      let open Experiment in
      let boolean_value fnc = fnc experiment |> bool_to_string |> txt in
      let default = "" in
      Message.
        [ ( Field.PublicTitle
          , experiment |> public_title |> PublicTitle.value |> txt )
        ; ( Field.ExperimentType
          , experiment
            |> experiment_type
            |> CCOption.map_or ~default ExperimentType.show
            |> txt )
        ; ( Field.InternalDescription
          , experiment
            |> internal_description
            |> CCOption.map_or
                 ~default:(txt "")
                 (InternalDescription.value %> HttpUtils.add_line_breaks) )
        ; ( Field.PublicDescription
          , experiment
            |> public_description
            |> CCOption.map_or
                 ~default:(txt "")
                 (PublicDescription.value %> HttpUtils.add_line_breaks) )
        ; ( Field.Language
          , experiment
            |> language
            |> CCOption.map_or ~default Language.show
            |> txt )
        ; ( Field.CostCenter
          , experiment
            |> cost_center
            |> CCOption.map_or ~default CostCenter.value
            |> txt )
        ; ( Field.OrganisationalUnit
          , experiment
            |> organisational_unit
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
          , experiment
            |> invitation_reset_at
            |> CCOption.map_or ~default:"-" InvitationResetAt.to_human
            |> txt )
        ]
      |> vertical_table
    in
    let statistics = Statistics.make language statistics in
    let message_template =
      div
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt (Utils.nav_link_to_string language I18n.MessageTemplates) ]
        ; Page_admin_message_template.(
            experiment_help
              ~entity:(Experiment experiment_id)
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
        ([ notifications
         ; div
             ~a:[ a_class [ "grid-col-3"; "align-start" ] ]
             [ div ~a:[ a_class [ "span-2" ] ] [ experiment_table ]
             ; div
                 ~a:[ a_class [ "border"; "inset"; "bg-grey-light" ] ]
                 [ statistics ]
             ]
         ; tag_overview
         ; message_template
         ]
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
        (build_experiment_path ~suffix:"edit" experiment)
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
                    (build_experiment_path ~suffix:"invitations/sent" experiment
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

let users
  ?hint
  ?can_assign
  ?can_unassign
  role
  experiment
  applicable_admins
  currently_assigned
  context
  =
  let base_url field admin =
    let suffix =
      Format.asprintf
        "%s/%s"
        (Field.show field)
        (Admin.id admin |> Admin.Id.value)
    in
    build_experiment_path ~suffix experiment |> Sihl.Web.externalize_path
  in
  let field =
    let open Message in
    match role with
    | `Assistants -> Field.Assistants
    | `Experimenter -> Field.Experimenter
  in
  Page_admin_experiment_users.role_assignment
    ?hint
    ?can_assign
    ?can_unassign
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
    let path suffix = build_experiment_path ~suffix experiment in
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
    tenant.Pool_tenant.text_messages_enabled
    action
    flash_fetcher
  |> CCList.return
  |> Layout.Experiment.create context (control_to_title control) experiment
;;

let message_history_url =
  build_experiment_path ~suffix:"messages" %> Uri.of_string
;;

let message_history context experiment messages =
  let open Pool_common in
  let html =
    Page_admin_message_history.list
      context
      (message_history_url experiment)
      messages
  in
  Layout.Experiment.(
    create
      ~active_navigation:I18n.MessageHistory
      context
      (NavLink I18n.MessageHistory)
      experiment
      [ html ])
;;
