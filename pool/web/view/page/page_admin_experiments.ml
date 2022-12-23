open Tyxml.Html
open Component
open Input
open Pool_common
module HttpUtils = Http_utils

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

let experiment_layout ?buttons ?hint language title experiment ?active html =
  let tab_links =
    I18n.
      [ Overview, "/"
      ; Field Message.Field.Assistants, "/assistants"
      ; Field Message.Field.Experimenter, "/experimenter"
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

let index experiment_list Pool_context.{ language; _ } =
  let thead =
    Message.
      [ Field.Title |> Table.field_to_txt language
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
        ; Format.asprintf
            "/admin/experiments/%s"
            (experiment.id |> Experiment.Id.value)
          |> edit_link
        ])
      experiment_list
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.ExperimentListTitle) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ Table.horizontal_table `Striped ~align_last_end:true ~thead rows ]
    ]
;;

let experiment_form
  ?experiment
  Pool_context.{ language; csrf; _ }
  sys_languages
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
      Message.Field.ExperimentType
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
      ]
    [ csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ input_element
            language
            `Text
            Message.Field.Title
            ~value:(value title_value)
            ~required:true
            ~flash_fetcher
        ; input_element
            language
            `Text
            Message.Field.PublicTitle
            ~value:(value public_title_value)
            ~required:true
            ~flash_fetcher
        ; textarea_element
            language
            Message.Field.Description
            ~value:(value description_value)
            ~required:true
            ~flash_fetcher
        ; experiment_type_select
        ]
    ; checkbox_element
        ~help:I18n.DirectRegistrationDisbled
        Message.Field.DirectRegistrationDisabled
        direct_registration_disabled_value
    ; checkbox_element
        ~help:I18n.RegistrationDisabled
        Message.Field.RegistrationDisabled
        registration_disabled_value
    ; checkbox_element
        ~help:I18n.AllowUninvitedSignup
        Message.Field.AllowUninvitedSignup
        allow_uninvited_signup_value
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-2" ] ]
            [ txt
                (Utils.field_to_string language Message.Field.Invitation
                |> CCString.capitalize_ascii)
            ]
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ MessageTextElements.experiment_invitation_help
                language
                ?experiment
                ()
            ; div
                ~a:[ a_class [ "grid-col-2" ] ]
                [ input_element
                    language
                    `Text
                    Message.Field.InvitationSubject
                    ~value:
                      (value (fun e ->
                         e.invitation_template
                         |> CCOption.map_or
                              ~default:""
                              InvitationTemplate.subject_value))
                    ~flash_fetcher
                ; textarea_element
                    language
                    Message.Field.InvitationText
                    ~value:
                      (value (fun e ->
                         e.invitation_template
                         |> CCOption.map_or
                              ~default:""
                              InvitationTemplate.text_value))
                    ~flash_fetcher
                ]
            ]
        ]
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-2" ] ]
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
                [ flatpicker_element
                    language
                    `Time
                    Message.Field.LeadTime
                    ~help:I18n.TimeSpanPickerHint
                    ~value:
                      (value (fun e ->
                         session_reminder_lead_time_value e
                         |> CCOption.map_or
                              ~default:""
                              Utils.Time.timespan_spanpicker))
                    ~flash_fetcher
                ; div
                    ~a:[ a_class [ "full-width" ] ]
                    [ MessageTextElements.session_reminder_help
                        language
                        sys_languages
                        ()
                    ]
                ; input_element
                    language
                    `Text
                    Message.Field.ReminderSubject
                    ~value:
                      (value (fun e ->
                         session_reminder_subject_value e
                         |> CCOption.value ~default:""))
                    ~flash_fetcher
                ; textarea_element
                    language
                    Message.Field.ReminderText
                    ~value:
                      (value (fun e ->
                         session_reminder_text_value e
                         |> CCOption.value ~default:""))
                    ~flash_fetcher
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

let create (Pool_context.{ language; _ } as context) sys_languages flash_fetcher
  =
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack" ] ]
    [ h1
        [ txt
            (Utils.control_to_string
               language
               Message.(Create (Some Field.Experiment)))
        ]
    ; experiment_form context sys_languages flash_fetcher
    ]
;;

let edit
  experiment
  (Pool_context.{ language; _ } as context)
  sys_languages
  invitation_templates
  session_reminder_templates
  flash_fetcher
  =
  let open Message_template in
  let form = experiment_form ~experiment context sys_languages flash_fetcher in
  let experiment_path =
    Format.asprintf
      "/admin/experiments/%s/%s"
      Experiment.(Id.value experiment.id)
  in
  let template_html label list =
    let edit_path m =
      Format.asprintf
        "%s/%s/edit"
        Message.Field.(human_url MessageTemplate)
        (Message_template.Id.value m.id)
      |> experiment_path
    in
    let new_path =
      if CCList.is_empty (Message_template.filter_languages sys_languages list)
      then None
      else experiment_path Label.(prefixed_human_url label) |> CCOption.pure
    in
    div
      [ h3 ~a:[ a_class [ "heading-2" ] ] [ txt (Label.to_human label) ]
      ; Page_admin_message_template.table language list new_path edit_path
      ]
  in
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ form
      ; template_html Label.ExperimentInvitation invitation_templates
      ; template_html Label.SessionReminder session_reminder_templates
      ]
  in
  experiment_layout
    language
    (Control Message.(Edit (Some Field.Experiment)))
    experiment
    html
;;

let detail experiment session_count Pool_context.{ language; csrf; _ } =
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
        ]
      |> vertical_table
    in
    let invitation_rows InvitationTemplate.{ subject; text } =
      let open InvitationTemplate in
      let table =
        Message.
          [ Field.InvitationSubject, subject |> Subject.value |> txt
          ; ( Field.InvitationText
            , text |> Text.value |> HttpUtils.add_line_breaks )
          ]
        |> vertical_table
      in
      div
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt
                (Utils.field_to_string language Message.Field.Invitation
                |> CCString.capitalize_ascii)
            ]
        ; table
        ]
    in
    let session_reminder_rows =
      let open Reminder in
      let open CCFun in
      let table =
        Message.
          [ ( Field.LeadTime
            , experiment.session_reminder_lead_time
              |> CCOption.map_or
                   ~default:""
                   (LeadTime.value %> Utils.Time.formatted_timespan)
              |> txt )
          ; ( Field.ReminderSubject
            , experiment.session_reminder_subject
              |> CCOption.map_or ~default:"" Subject.value
              |> txt )
          ; ( Field.ReminderText
            , experiment.session_reminder_text
              |> CCOption.map_or
                   ~default:(txt "")
                   (Text.value %> HttpUtils.add_line_breaks) )
          ]
        |> vertical_table
      in
      div
        [ h3
            ~a:[ a_class [ "heading-3" ] ]
            [ txt
                (Utils.text_to_string language I18n.SessionReminder
                |> CCString.capitalize_ascii)
            ]
        ; table
        ]
    in
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ experiment_table
      ; experiment.invitation_template
        |> CCOption.map_or ~default:(txt "") invitation_rows
      ; session_reminder_rows
      ; delete_form
      ]
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
    Page_admin_invitations.Partials.list context experiment invitations
  in
  experiment_layout
    language
    (I18n I18n.SentInvitations)
    experiment
    ~active:I18n.Invitations
    html
;;

let waiting_list waiting_list experiment Pool_context.{ language; _ } =
  let open Waiting_list.ExperimentList in
  let waiting_list_entries () =
    let thead =
      (Message.Field.[ Name; Email; CreatedAt; Comment ]
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
          ; entry.comment
            |> CCOption.map_or ~default:"" Waiting_list.Comment.value
            |> HttpUtils.add_line_breaks
          ; Format.asprintf
              "/admin/experiments/%s/waiting-list/%s"
              (waiting_list.experiment.Experiment.id |> Experiment.Id.value)
              (entry.id |> Id.value)
            |> edit_link
          ])
        waiting_list.waiting_list_entries
    in
    Table.horizontal_table `Striped ~align_last_end:true ~thead rows
  in
  experiment_layout
    ~hint:I18n.ExperimentWaitingList
    language
    (NavLink I18n.WaitingList)
    experiment
    ~active:I18n.WaitingList
    (waiting_list_entries ())
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
      (Message.Field.show field)
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
    ~divest:"divest"
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
    | Some template ->
      Format.asprintf
        "%s/%s"
        Message.Field.(human_url MessageTemplate)
        (Id.value template.id)
      |> go
  in
  let title =
    (match template with
     | None -> "Create"
     | Some _ -> "Edit")
    |> fun control ->
    String
      (Format.asprintf
         "%s %s"
         control
         (label |> Label.to_human |> CCString.lowercase_ascii))
  in
  Page_admin_message_template.template_form
    context
    ?languages
    template
    action
    flash_fetcher
  |> experiment_layout language title experiment
;;
