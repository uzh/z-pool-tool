open Tyxml.Html
open Component
module Message = Pool_common.Message

type title =
  | Control of Pool_common.Message.control
  | NavLink of Pool_common.I18n.nav_link
  | I18n of Pool_common.I18n.t

let title_to_string language text =
  let open Pool_common.Utils in
  match text with
  | Control text -> control_to_string language text
  | NavLink text -> nav_link_to_string language text
  | I18n text -> text_to_string language text
;;

let experiment_layout language title experiment ?active html =
  let subnav_links =
    Pool_common.I18n.
      [ Overview, "/"
      ; Invitations, "/invitations"
      ; WaitingList, "/waiting-list"
      ; Sessions, "/sessions"
      ; Assignments, "/assignments"
      ; Mailings, "/mailings"
      ]
  in
  let base_url =
    Format.asprintf
      "/admin/experiments/%s"
      (Pool_common.Id.value experiment.Experiment.id)
  in
  let open Experiment in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (experiment.title |> Title.value) ]
    ; Component.Navigation.subnav language subnav_links base_url active
    ; h2 ~a:[ a_class [ "heading-2" ] ] [ txt (title_to_string language title) ]
    ; html
    ]
;;

let index experiment_list Pool_context.{ language; _ } =
  let thead = Pool_common.Message.Field.[ Some Title; None ] in
  let rows =
    CCList.map
      (fun (experiment : Experiment.t) ->
        let open Experiment in
        [ txt (Title.value experiment.title)
        ; a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/admin/experiments/%s"
                        (experiment.id |> Pool_common.Id.value)))
              ]
            [ txt Pool_common.(Message.More |> Utils.control_to_string language)
            ]
        ])
      experiment_list
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.text_to_string language I18n.ExperimentListTitle)
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ Table.horizontal_table `Striped language ~thead rows
        ; p
            [ a
                ~a:
                  [ a_href
                      (Sihl.Web.externalize_path "/admin/experiments/create")
                  ]
                [ txt
                    Pool_common.(
                      Message.(Add (Some Field.Experiment))
                      |> Utils.control_to_string language)
                ]
            ]
        ]
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
        (experiment.id |> Pool_common.Id.value)
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") experiment in
  let experiment_type_select =
    let open Pool_common.ExperimentType in
    Component.selector
      language
      Pool_common.Message.Field.ExperimentType
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
    [ Component.csrf_element csrf ()
    ; input_element
        language
        `Text
        Pool_common.Message.Field.Title
        ~value:(value title_value)
        ~required:true
        ~flash_fetcher
    ; input_element
        language
        `Text
        Pool_common.Message.Field.publictitle
        ~value:(value public_title_value)
        ~required:true
        ~flash_fetcher
    ; textarea_element
        language
        Pool_common.Message.Field.Description
        ~value:(value description_value)
        ~required:true
        ~flash_fetcher
    ; div ~a:[ a_class [ "switcher" ] ] [ experiment_type_select; div [] ]
    ; checkbox_element
        language
        ~help:Pool_common.I18n.DirectRegistrationDisbled
        Pool_common.Message.Field.DirectRegistrationDisabled
        ~value:
          (experiment
          |> CCOption.map_or ~default:false direct_registration_disabled_value)
        ~flash_fetcher
    ; checkbox_element
        language
        ~help:Pool_common.I18n.RegistrationDisabled
        Pool_common.Message.Field.RegistrationDisabled
        ~value:
          (experiment
          |> CCOption.map_or ~default:false registration_disabled_value)
        ~flash_fetcher
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-2" ] ]
            [ txt
                Pool_common.(
                  Utils.field_to_string language Message.Field.Invitation
                  |> CCString.capitalize_ascii)
            ]
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ MessageTextElements.experiment_invitation_help
                language
                ?experiment
                ()
            ; input_element
                language
                `Text
                Pool_common.Message.Field.InvitationSubject
                ~value:
                  (value (fun e ->
                       e.invitation_template
                       |> CCOption.map_or
                            ~default:""
                            InvitationTemplate.subject_value))
                ~flash_fetcher
            ; textarea_element
                language
                Pool_common.Message.Field.InvitationText
                ~value:
                  (value (fun e ->
                       e.invitation_template
                       |> CCOption.map_or
                            ~default:""
                            InvitationTemplate.text_value))
                ~flash_fetcher
            ]
        ]
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        [ h3
            ~a:[ a_class [ "heading-2" ] ]
            [ txt
                Pool_common.(Utils.text_to_string language I18n.SessionReminder)
            ]
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ p
                [ txt
                    Pool_common.(
                      Utils.text_to_string
                        language
                        I18n.ExperimentSessionReminderHint)
                ]
            ; flatpicker_element
                language
                `Time
                Pool_common.Message.Field.LeadTime
                ~help:Pool_common.I18n.TimeSpanPickerHint
                ~value:
                  (value (fun e ->
                       session_reminder_lead_time_value e
                       |> CCOption.map_or
                            ~default:""
                            Pool_common.Utils.Time.timespan_spanpicker))
                ~flash_fetcher
            ; div
                ~a:[ a_class [ "stack" ] ]
                [ MessageTextElements.session_reminder_help
                    language
                    sys_languages
                    ()
                ; input_element
                    language
                    `Text
                    Pool_common.Message.Field.ReminderSubject
                    ~value:
                      (value (fun e ->
                           session_reminder_subject_value e
                           |> CCOption.value ~default:""))
                    ~flash_fetcher
                ; textarea_element
                    language
                    Pool_common.Message.Field.ReminderText
                    ~value:
                      (value (fun e ->
                           session_reminder_text_value e
                           |> CCOption.value ~default:""))
                    ~flash_fetcher
                ]
            ]
        ]
    ; submit_element
        language
        Message.(
          let field = Some Field.Experiment in
          match experiment with
          | None -> Create field
          | Some _ -> Update field)
        ~submit_type:`Success
        ()
    ]
;;

let create (Pool_context.{ language; _ } as context) sys_languages flash_fetcher
  =
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure"; "stack" ] ]
    [ h1
        [ txt
            Pool_common.(
              Utils.control_to_string
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
    flash_fetcher
  =
  let html = experiment_form ~experiment context sys_languages flash_fetcher in
  experiment_layout
    language
    (Control Pool_common.Message.(Edit (Some Field.Experiment)))
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
            ()
        ; p
            [ small
                [ txt
                    Pool_common.(
                      Message.ExperimentSessionCountNotZero
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
                    (experiment.Experiment.id |> Pool_common.Id.value)))
          ; a_user_data
              "confirmable"
              Pool_common.(
                Utils.confirmable_to_string language I18n.DeleteExperiment)
          ]
        [ csrf_element csrf ()
        ; submit_element
            language
            Message.(Delete (Some Field.Experiment))
            ~submit_type:`Error
            ~has_icon:`TrashOutline
            ()
        ]
  in
  let bool_to_string = Pool_common.Utils.bool_to_string language in
  let open Experiment in
  let html =
    let rows =
      let open Experiment in
      Message.Field.
        [ DirectRegistrationDisabled, direct_registration_disabled_value
        ; RegistrationDisabled, registration_disabled_value
        ]
      |> CCList.map (fun (label, fnc) ->
             label, fnc experiment |> bool_to_string |> txt)
    in
    div
      [ p
          [ experiment.description
            |> Description.value
            |> Http_utils.add_line_breaks
          ]
      ; Table.vertical_table `Striped language rows
      ; p
          [ a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/admin/experiments/%s/edit"
                          (experiment.id |> Pool_common.Id.value)))
                ]
              [ txt
                  Pool_common.(
                    Message.(Edit (Some Field.Experiment))
                    |> Utils.control_to_string language)
              ]
          ]
      ; delete_form
      ]
  in
  experiment_layout
    language
    (NavLink Pool_common.I18n.Overview)
    experiment
    ~active:Pool_common.I18n.Overview
    html
;;

let invitations
    invitations
    experiment
    filtered_contacts
    (Pool_context.{ language; _ } as context)
  =
  let html =
    div
      ~a:[ a_class [ "stack-lg" ] ]
      [ Page_admin_invitations.Partials.list context experiment invitations
      ; Page_admin_invitations.Partials.send_invitation
          context
          experiment
          filtered_contacts
      ]
  in
  experiment_layout
    language
    (NavLink Pool_common.I18n.Invitations)
    experiment
    ~active:Pool_common.I18n.Invitations
    html
;;

let waiting_list waiting_list experiment Pool_context.{ language; _ } =
  let open Waiting_list.ExperimentList in
  let waiting_list_entries () =
    let thead =
      Pool_common.Message.Field.
        [ Some Name; Some Email; Some CreatedAt; Some Comment; None ]
    in
    let rows =
      CCList.map
        (fun entry ->
          [ txt (Contact.Preview.fullname entry.contact)
          ; txt
              (Contact.Preview.email_address entry.contact
              |> Pool_user.EmailAddress.value)
          ; txt
              Pool_common.(
                entry.created_at
                |> CreatedAt.value
                |> Utils.Time.formatted_date_time)
          ; entry.comment
            |> CCOption.map_or ~default:"" Waiting_list.Comment.value
            |> Http_utils.add_line_breaks
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf
                          "/admin/experiments/%s/waiting-list/%s"
                          (waiting_list.experiment.Experiment.id
                          |> Pool_common.Id.value)
                          (entry.id |> Pool_common.Id.value)))
                ]
              [ txt
                  Pool_common.(Message.More |> Utils.control_to_string language)
              ]
          ])
        waiting_list.waiting_list_entries
    in
    Component.Table.horizontal_table `Striped language ~thead rows
  in
  experiment_layout
    language
    (NavLink Pool_common.I18n.WaitingList)
    experiment
    ~active:Pool_common.I18n.WaitingList
    (waiting_list_entries ())
;;
