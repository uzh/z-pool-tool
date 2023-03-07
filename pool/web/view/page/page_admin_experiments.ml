open Tyxml.Html
open Component
open Input
open Pool_common
module HttpUtils = Http_utils
module Field = Message.Field

type title =
  | Control of Message.control
  | NavLink of I18n.nav_link
  | I18n of I18n.t
  | String of string

let title_to_string language text =
  let open Utils in
  match text with
  | Control text -> control_to_string language text
  | NavLink text -> nav_link_to_string language text
  | I18n text -> text_to_string language text
  | String str -> str
;;

let build_experiment_path experiment =
  Format.asprintf "/admin/experiments/%s/%s" Experiment.(Id.value experiment.id)
;;

let experiment_layout ?buttons ?hint language title experiment ?active html =
  let tab_links =
    I18n.
      [ Overview, "/"
      ; Field Field.Assistants, "/assistants"
      ; Field Field.Experimenter, "/experimenter"
      ; Invitations, "/invitations"
      ; WaitingList, "/waiting-list"
      ; Sessions, "/sessions"
      ; Assignments, "/assignments"
      ; Mailings, "/mailings"
      ]
    |> CCList.map (fun (label, url) ->
         ( label
         , Format.asprintf
             "/admin/experiments/%s/%s"
             (Experiment.Id.value experiment.Experiment.id)
             url ))
  in
  let title =
    let base =
      h2 ~a:[ a_class [ "heading-2" ] ] [ txt (title_to_string language title) ]
    in
    let title =
      match buttons with
      | None -> base
      | Some btns ->
        div
          ~a:
            [ a_class
                [ "flexrow"
                ; "justify-between"
                ; "flex-gap"
                ; "flexcolumn-mobile"
                ]
            ]
          [ div [ base ]; div [ btns ] ]
    in
    match hint with
    | None -> [ title ]
    | Some hint ->
      [ title
      ; p [ Utils.hint_to_string language hint |> HttpUtils.add_line_breaks ]
      ]
  in
  let html = title @ [ div ~a:[ a_class [ "gap-lg" ] ] [ html ] ] in
  let open Experiment in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (experiment.title |> Title.value) ]
    ; Navigation.tab_navigation language tab_links active html
    ]
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

let index experiment_list Pool_context.{ language; _ } =
  let experiment_table experiments =
    let thead =
      Message.
        [ Field.Title |> Table.field_to_txt language
        ; Field.PublicTitle |> Table.field_to_txt language
        ; link_as_button
            ~style:`Success
            ~icon:`Add
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
            ~value:(value description_value)
            ~required:true
            ~flash_fetcher
        ; experiment_type_select
        ]
    ; checkbox_element
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
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
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
                             |> CCOption.map Pool_common.Reminder.LeadTime.value))
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
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element
            ~classnames:[ "push" ]
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
;;

let create
  (Pool_context.{ language; _ } as context)
  default_reminder_lead_time
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
    ; experiment_form context default_reminder_lead_time flash_fetcher
    ]
;;

let edit
  experiment
  (Pool_context.{ language; _ } as context)
  sys_languages
  default_reminder_lead_time
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
    experiment_form ~experiment context default_reminder_lead_time flash_fetcher
  in
  let experiment_path =
    Format.asprintf
      "/admin/experiments/%s/%s"
      Experiment.(Id.value experiment.id)
  in
  let message_templates_html =
    message_templates_html language experiment_path sys_languages
  in
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ notifications
      ; form
      ; message_templates_html Label.ExperimentInvitation invitation_templates
      ; message_templates_html Label.SessionReminder session_reminder_templates
      ]
  in
  experiment_layout
    language
    (Control Message.(Edit (Some Field.Experiment)))
    experiment
    html
;;

let detail
  experiment
  session_count
  invitation_templates
  session_reminder_templates
  sys_languages
  Pool_context.{ language; csrf; _ }
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
            ~has_icon:`TrashOutline
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
            |> Description.value
            |> HttpUtils.add_line_breaks )
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
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ notifications; experiment_table; message_template; delete_form ]
  in
  let edit_button =
    link_as_button
      ~icon:`Create
      ~classnames:[ "small" ]
      ~control:(language, Message.(Edit (Some Field.Experiment)))
      (Format.asprintf
         "/admin/experiments/%s/edit"
         (experiment.id |> Experiment.Id.value))
  in
  experiment_layout
    ~buttons:edit_button
    language
    (NavLink I18n.Overview)
    experiment
    ~active:I18n.Overview
    html
;;

let invitations
  experiment
  key_list
  template_list
  query_experiments
  filtered_contacts
  (Pool_context.{ language; _ } as context)
  =
  let html =
    div
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
  in
  experiment_layout
    language
    (NavLink I18n.Invitations)
    experiment
    ~active:I18n.Invitations
    html
;;

let sent_invitations
  (Pool_context.{ language; _ } as context)
  experiment
  invitations
  =
  let html =
    let invitation_table =
      Page_admin_invitations.Partials.list context experiment
    in
    Component.List.create
      language
      invitation_table
      Invitation.sortable_by
      Invitation.searchable_by
      invitations
  in
  experiment_layout
    language
    (I18n I18n.SentInvitations)
    experiment
    ~active:I18n.Invitations
    html
;;

let waiting_list
  ({ Waiting_list.ExperimentList.experiment; waiting_list_entries }, query)
  Pool_context.{ language; _ }
  =
  let open Waiting_list.ExperimentList in
  let waiting_list_table waiting_list_entries =
    let thead =
      (Field.[ Name; Email; CreatedAt; AdminComment ]
       |> Table.fields_to_txt language)
      @ [ txt "" ]
    in
    let rows =
      CCList.map
        (fun entry ->
          [ txt (Contact.Preview.fullname entry.contact)
          ; txt
              (Contact.Preview.email_address entry.contact
               |> Pool_user.EmailAddress.value)
          ; txt
              (entry.created_at
               |> CreatedAt.value
               |> Utils.Time.formatted_date_time)
          ; entry.admin_comment
            |> CCOption.map_or ~default:"" Waiting_list.AdminComment.value
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
  let html =
    Component.List.create
      language
      waiting_list_table
      Waiting_list.sortable_by
      Waiting_list.searchable_by
      (waiting_list_entries, query)
  in
  experiment_layout
    ~hint:I18n.ExperimentWaitingList
    language
    (NavLink I18n.WaitingList)
    experiment
    ~active:I18n.WaitingList
    html
;;

let users
  role
  experiment
  applicable_admins
  currently_assigned
  (Pool_context.{ language; _ } as context)
  =
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
  |> experiment_layout
       language
       (NavLink (I18n.Field field))
       experiment
       ~active:(I18n.Field field)
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
    (match template with
     | None -> Message.(Create None)
     | Some _ -> Message.(Edit None))
    |> fun control ->
    String
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
  |> experiment_layout language title experiment
;;
