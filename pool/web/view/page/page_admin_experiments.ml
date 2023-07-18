open Tyxml.Html
open Component
open Input
open Pool_common
module HttpUtils = Http_utils
module Field = Message.Field

let build_experiment_path experiment =
  Format.asprintf "/admin/experiments/%s/%s" Experiment.(Id.value experiment.id)
;;

let notifications
  language
  sys_languages
  invitation_templates
  session_reminder_templates
  =
  let open CCList in
  let open Pool_common in
  let open Message_template in
  Label.
    [ invitation_templates, ExperimentInvitation
    ; session_reminder_templates, SessionReminder
    ]
  |> filter_map (fun (templates, label) ->
       if is_empty templates
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
         | langs ->
           I18n.MissingMessageTemplates
             (Label.to_human label, CCList.map Language.show langs)
           |> Utils.hint_to_string language
           |> txt
           |> pure
           |> Notification.notification language `Warning
           |> CCOption.return)
  |> function
  | [] -> txt ""
  | notifications -> div ~a:[ a_class [ "stack" ] ] notifications
;;

let message_templates_html
  ?(title =
    fun label ->
      h2
        ~a:[ a_class [ "heading-2" ] ]
        [ txt (Message_template.Label.to_human label) ])
  language
  experiment_path
  sys_languages
  label
  list
  =
  let open Message_template in
  let edit_path =
    CCFun.(prefixed_template_url ~append:"edit" %> experiment_path)
  in
  let new_path =
    if CCList.is_empty (filter_languages sys_languages list)
    then None
    else experiment_path Label.(prefixed_human_url label) |> CCOption.pure
  in
  div
    [ title label
    ; Page_admin_message_template.table language list new_path edit_path
    ; p
        ~a:[ a_class [ "gap-sm" ] ]
        [ txt
            (if CCList.is_empty list
             then
               Pool_common.(
                 Utils.text_to_string
                   language
                   (I18n.NoEntries Field.MessageTemplates))
             else "")
        ]
    ]
;;

let index Pool_context.{ language; _ } experiment_list =
  let experiment_table experiments =
    let thead =
      Message.
        [ Field.Title |> Table.field_to_txt language
        ; Field.PublicTitle |> Table.field_to_txt language
        ; link_as_button
            ~style:`Success
            ~icon:Icon.Add
            ~control:(language, Message.(Add (Some Field.Experiment)))
            "/admin/experiments/create"
        ]
    in
    let rows =
      CCList.map
        (fun (experiment : Experiment.t) ->
          let open Experiment in
          [ txt (Title.value experiment.title)
          ; txt (PublicTitle.value experiment.public_title)
          ; Format.asprintf
              "/admin/experiments/%s"
              (experiment.id |> Experiment.Id.value)
            |> edit_link
          ])
        experiments
    in
    Table.horizontal_table `Striped ~align_last_end:true ~thead rows
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.ExperimentListTitle) ]
    ; Component.List.create
        language
        experiment_table
        Experiment.sortable_by
        Experiment.searchable_by
        experiment_list
    ]
;;

let experiment_form
  ?experiment
  Pool_context.{ language; csrf; _ }
  contact_persons
  organisational_units
  smtp_auth_list
  default_reminder_lead_time
  flash_fetcher
  =
  let open Experiment in
  let action =
    match experiment with
    | None -> "/admin/experiments"
    | Some experiment ->
      Format.asprintf
        "/admin/experiments/%s"
        (experiment.id |> Experiment.Id.value)
  in
  let checkbox_element ?help ?(default = false) field fnc =
    checkbox_element
      language
      ?help
      field
      ~value:(experiment |> CCOption.map_or ~default fnc)
      ~flash_fetcher
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") experiment in
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
                ~required:true
                ~flash_fetcher
            ; textarea_element
                language
                Field.Description
                ?value:
                  (CCOption.bind experiment (fun { description; _ } ->
                     description |> CCOption.map Description.value))
                ~flash_fetcher
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
                    ~help:Pool_common.I18n.ExperimentContactPerson
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
                ~help:I18n.DirectRegistrationDisbled
                Field.DirectRegistrationDisabled
                direct_registration_disabled_value
            ; checkbox_element
                ~help:I18n.RegistrationDisabled
                Field.RegistrationDisabled
                registration_disabled_value
            ; checkbox_element
                ~help:I18n.AllowUninvitedSignup
                Field.AllowUninvitedSignup
                allow_uninvited_signup_value
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
                    [ div
                        [ timespan_picker
                            language
                            Field.LeadTime
                            ~help:I18n.TimeSpanPickerHint
                            ?value:
                              (CCOption.bind experiment (fun (e : t) ->
                                 e.session_reminder_lead_time
                                 |> CCOption.map
                                      Pool_common.Reminder.LeadTime.value))
                            ~flash_fetcher
                        ; Utils.text_to_string
                            language
                            (I18n.SessionReminderDefaultLeadTime
                               (default_reminder_lead_time
                                |> Reminder.LeadTime.value))
                          |> txt
                          |> HttpUtils.default_value_style
                        ]
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
  default_reminder_lead_time
  contact_persons
  smtp_auth_list
  flash_fetcher
  =
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
        default_reminder_lead_time
        flash_fetcher
    ]
;;

let edit
  experiment
  ({ Pool_context.language; _ } as context)
  sys_languages
  default_reminder_lead_time
  contact_persons
  organisational_units
  smtp_auth_list
  invitation_templates
  session_reminder_templates
  flash_fetcher
  =
  let open Message_template in
  let notifications =
    notifications
      language
      sys_languages
      invitation_templates
      session_reminder_templates
  in
  let form =
    experiment_form
      ~experiment
      context
      contact_persons
      organisational_units
      smtp_auth_list
      default_reminder_lead_time
      flash_fetcher
  in
  let experiment_path =
    Format.asprintf
      "/admin/experiments/%s/%s"
      Experiment.(Id.value experiment.id)
  in
  let message_templates_html =
    message_templates_html language experiment_path sys_languages
  in
  [ div
      ~a:[ a_class [ "stack-lg" ] ]
      [ notifications
      ; form
      ; message_templates_html Label.ExperimentInvitation invitation_templates
      ; message_templates_html Label.SessionReminder session_reminder_templates
      ]
  ]
  |> Layout.Experiment.(
       create
         context
         (Control Message.(Edit (Some Field.Experiment)))
         experiment)
;;

let detail
  experiment
  session_count
  invitation_templates
  session_reminder_templates
  sys_languages
  contact_person
  smtp_account
  ({ Pool_context.language; csrf; _ } as context)
  =
  let experiment_path = build_experiment_path experiment in
  let notifications =
    notifications
      language
      sys_languages
      invitation_templates
      session_reminder_templates
  in
  let delete_form =
    match session_count > 0 with
    | true ->
      div
        [ submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~submit_type:`Disabled
            ~classnames:[ "small" ]
            ()
        ; p
            [ small
                [ txt
                    (Message.ExperimentSessionCountNotZero
                     |> Utils.error_to_string language)
                ]
            ]
        ]
    | false ->
      Tyxml.Html.form
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
      Message.
        [ Field.PublicTitle, experiment.public_title |> PublicTitle.value |> txt
        ; ( Field.ExperimentType
          , experiment.experiment_type
            |> CCOption.map_or ~default:"" ExperimentType.show
            |> txt )
        ; ( Field.Description
          , experiment.description
            |> CCOption.map_or ~default:(txt "") (fun desc ->
                 desc |> Description.value |> HttpUtils.add_line_breaks) )
        ; ( Field.CostCenter
          , experiment.cost_center
            |> CCOption.map_or ~default:"" CostCenter.value
            |> txt )
        ; ( Field.OrganisationalUnit
          , experiment.organisational_unit
            |> CCOption.map_or
                 ~default:""
                 Organisational_unit.(fun ou -> ou.name |> Name.value)
            |> txt )
        ; ( Field.ContactPerson
          , contact_person |> CCOption.map_or ~default:"" Admin.full_name |> txt
          )
        ; ( Field.Smtp
          , smtp_account
            |> CCOption.map_or
                 ~default:""
                 Email.SmtpAuth.(fun auth -> auth.label |> Label.value)
            |> txt )
        ; ( Field.DirectRegistrationDisabled
          , direct_registration_disabled_value |> boolean_value )
        ; ( Field.RegistrationDisabled
          , registration_disabled_value |> boolean_value )
        ; ( Field.AllowUninvitedSignup
          , allow_uninvited_signup_value |> boolean_value )
        ; ( Field.ExperimentReminderLeadTime
          , session_reminder_lead_time_value experiment
            |> CCOption.map_or
                 ~default:"-"
                 Pool_common.Utils.Time.formatted_timespan
            |> txt )
        ]
      |> vertical_table
    in
    let message_template =
      let open Message_template in
      let list =
        let title label = h4 [ txt (Label.to_human label) ] in
        message_templates_html ~title language experiment_path sys_languages
      in
      div
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt
                Pool_common.(
                  Utils.nav_link_to_string language I18n.MessageTemplates)
            ]
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ list Label.ExperimentInvitation invitation_templates
            ; list Label.SessionReminder session_reminder_templates
            ]
        ]
    in
    [ div
        ~a:[ a_class [ "stack-lg" ] ]
        [ notifications; experiment_table; message_template; delete_form ]
    ]
  in
  let edit_button =
    link_as_button
      ~icon:Icon.Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Experiment)))
      (Format.asprintf
         "/admin/experiments/%s/edit"
         (experiment.id |> Experiment.Id.value))
  in
  Layout.Experiment.(
    create
      ~active_navigation:I18n.Overview
      ~buttons:edit_button
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
  filtered_contacts
  ({ Pool_context.language; _ } as context)
  =
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
          filtered_contacts
      ]
  ]
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Invitations
         context
         (NavLink I18n.Invitations)
         experiment)
;;

let sent_invitations
  (Pool_context.{ language; _ } as context)
  experiment
  invitations
  =
  let invitation_table =
    Page_admin_invitations.Partials.list context experiment
  in
  Component.List.create
    language
    invitation_table
    Invitation.sortable_by
    Invitation.searchable_by
    invitations
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.Invitations
         context
         (I18n I18n.SentInvitations)
         experiment)
;;

let waiting_list
  ({ Waiting_list.ExperimentList.experiment; waiting_list_entries }, query)
  ({ Pool_context.language; _ } as context)
  =
  let open Waiting_list.ExperimentList in
  let waiting_list_table waiting_list_entries =
    let thead =
      (Field.[ Name; Email; CellPhone; SignedUpAt; AdminComment ]
       |> Table.fields_to_txt language)
      @ [ txt "" ]
    in
    let rows =
      let open CCOption in
      CCList.map
        Contact.Preview.(
          fun entry ->
            [ txt (fullname entry.contact)
            ; txt (email_address entry.contact |> Pool_user.EmailAddress.value)
            ; txt
                (entry.contact.cell_phone
                 |> map_or ~default:"" Pool_user.CellPhone.value)
            ; txt
                (entry.created_at
                 |> CreatedAt.value
                 |> Utils.Time.formatted_date_time)
            ; entry.admin_comment
              |> map_or ~default:"" Waiting_list.AdminComment.value
              |> HttpUtils.first_n_characters
              |> HttpUtils.add_line_breaks
            ; Format.asprintf
                "/admin/experiments/%s/waiting-list/%s"
                (experiment.Experiment.id |> Experiment.Id.value)
                (entry.id |> Id.value)
              |> edit_link
            ])
        waiting_list_entries
    in
    Table.horizontal_table
      `Striped
      ~align_top:true
      ~align_last_end:true
      ~thead
      rows
  in
  Component.List.create
    language
    waiting_list_table
    Waiting_list.sortable_by
    Waiting_list.searchable_by
    (waiting_list_entries, query)
  |> CCList.return
  |> Layout.Experiment.(
       create
         ~active_navigation:I18n.WaitingList
         ~hint:I18n.ExperimentWaitingList
         context
         (NavLink I18n.WaitingList)
         experiment)
;;

let users role experiment applicable_admins currently_assigned context =
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
         ~active_navigation:(I18n.Field field)
         context
         (NavLink (I18n.Field field))
         experiment)
;;

let message_template_form
  ({ Pool_context.language; _ } as context)
  tenant
  experiment
  languages
  label
  template
  flash_fetcher
  =
  let open Message_template in
  let action =
    let go =
      Format.asprintf
        "/admin/experiments/%s/%s"
        Experiment.(Id.value experiment.Experiment.id)
    in
    match template with
    | None -> go (Label.prefixed_human_url label)
    | Some template -> prefixed_template_url template |> go
  in
  let title =
    let open Pool_common in
    let open Layout.Experiment in
    (match template with
     | None -> Message.(Create None)
     | Some _ -> Message.(Edit None))
    |> fun control ->
    Text
      (Format.asprintf
         "%s %s"
         (control |> Utils.control_to_string language)
         (label |> Label.to_human |> CCString.lowercase_ascii))
  in
  let text_elements =
    Component.MessageTextElements.message_template_help
      ~experiment
      language
      tenant
      label
  in
  Page_admin_message_template.template_form
    context
    ?languages
    ~text_elements
    template
    action
    flash_fetcher
  |> CCList.return
  |> Layout.Experiment.create context title experiment
;;
