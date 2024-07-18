open Containers
open CCFun
open Tyxml.Html
module DataTable = Component.DataTable
module Table = Component.Table
module Input = Component.Input
module Icon = Component.Icon
module Modal = Component.Modal
module Status = Component.UserStatus.Contact

let path =
  Contact.id %> Contact.Id.value %> Format.asprintf "/admin/contacts/%s"
;;

let contact_lastname_firstname access_contact_profiles contact =
  let text = contact |> Contact.lastname_firstname |> txt in
  match access_contact_profiles with
  | true -> a ~a:[ a_href (path contact |> Sihl.Web.externalize_path) ] [ text ]
  | false -> text
;;

let enroll_contact_modal_id = "enroll-modal"

let enroll_contact_path ?suffix contact_id =
  Format.asprintf
    "/admin/contacts/%s/%s"
    (Contact.Id.value contact_id)
    Pool_message.Field.(Experiments |> human_url)
  |> (fun base ->
       match suffix with
       | None -> base
       | Some suffix -> Format.asprintf "%s/%s" base suffix)
  |> Sihl.Web.externalize_path
;;

let heading_with_icons contact =
  h1
    ~a:[ a_class [ "heading-1" ] ]
    [ Status.identity_with_icons
        Pool_common.Language.En
        ~context:`All
        true
        contact
    ]
;;

let personal_detail
  ?admin_comment
  ?custom_fields
  ?tags
  current_user
  language
  contact
  =
  let open Contact in
  let open Pool_message in
  let field_to_string =
    CCFun.(
      Pool_common.Utils.field_to_string language %> CCString.capitalize_ascii)
  in
  let with_comment =
    match admin_comment with
    | None -> []
    | Some comment ->
      [ ( field_to_string Field.AdminComment
        , comment |> AdminComment.value |> Http_utils.add_line_breaks )
      ]
  in
  let tags =
    tags
    |> CCOption.map_or ~default:[] (fun tags ->
      [ field_to_string Field.Tags, Component.Tag.tag_list language tags ])
  in
  let custom_field_rows =
    let open Custom_field in
    let open CCList in
    let field_to_row custom_field =
      let field_name = Public.name_value language custom_field in
      tr
        [ th [ txt field_name ]
        ; td
            [ Component.CustomField.answer_to_html
                current_user
                language
                custom_field
            ]
        ]
    in
    custom_fields
    |> CCOption.map_or ~default:[] (fun (grouped, ungrouped) ->
      let ungrouped = ungrouped >|= field_to_row in
      let grouped =
        grouped
        |> flat_map (fun { Group.Public.fields; _ } -> fields >|= field_to_row)
      in
      ungrouped @ grouped)
  in
  Pool_message.(
    [ field_to_string Field.Name, fullname contact |> txt
    ; ( field_to_string Field.Email
      , email_address contact |> Pool_user.EmailAddress.value |> txt )
    ; ( field_to_string Field.CellPhone
      , contact.cell_phone
        |> CCOption.map_or ~default:"" Pool_user.CellPhone.value
        |> txt )
    ; ( field_to_string Field.Language
      , contact.language
        |> CCOption.map_or ~default:"" Pool_common.Language.show
        |> txt )
    ]
    @ with_comment
    @ tags)
  |> fun rows ->
  let rows =
    CCList.map
      (fun (label, value) -> tr [ th [ txt label ]; td [ value ] ])
      rows
    @ custom_field_rows
  in
  table ~a:[ a_class (Table.table_classes `Striped ~align_top:true ()) ] rows
  |> fun html ->
  div
    [ h3
        ~a:[ a_class [ "heading-3" ] ]
        [ Pool_common.(
            Utils.nav_link_to_string language I18n.PersonalDetails |> txt)
        ]
    ; html
    ]
;;

let assign_contact_experiment_modal
  { Pool_context.language; csrf; _ }
  contact_id
  (experiment : Experiment.t)
  sessions
  matches_filter
  =
  let open Pool_common in
  let title language = Utils.text_to_string language I18n.EnrollInExperiment in
  let registration_disabled =
    Experiment.(experiment.registration_disabled |> RegistrationDisabled.value)
  in
  let notification =
    I18n.
      [ not matches_filter, ContactEnrollmentDoesNotMatchFilter
      ; registration_disabled, ContactEnrollmentRegistrationDisabled
      ]
    |> CCList.filter_map (fun (show, i18n) ->
      if show
      then Some (p [ Utils.hint_to_string language i18n |> txt ])
      else None)
    |> function
    | [] -> txt ""
    | msg -> Component.Notification.notification language `Error msg
  in
  let session_select =
    let label = Session.start_end_with_duration_human in
    let follow_up_row session = div [ txt (label session) ] in
    let row (session, follow_ups) =
      let tooltip, attribs =
        Session.assignment_creatable session
        |> function
        | Ok () ->
          ( txt ""
          , [ a_name Pool_message.Field.(show Session)
            ; a_value Session.(Id.value session.id)
            ; a_required ()
            ] )
        | Error msg ->
          ( msg |> Utils.error_to_string language |> Component.Tooltip.create
          , [ a_disabled () ] )
      in
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-xs" ] ]
        [ span [ input ~a:(a_input_type `Radio :: attribs) () ]
        ; div
            ~a:[ a_class [ "grow"; "flexcolumn"; "stack-xs" ] ]
            [ div
                ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
                [ span [ txt (label session) ]; tooltip ]
            ; div
                ~a:[ a_class [ "inset"; " left"; "hide-empty" ] ]
                (CCList.map follow_up_row follow_ups)
            ]
        ]
    in
    sessions |> CCList.map row |> div ~a:[ a_class [ "stack" ] ]
  in
  let form =
    div
      [ h4
          [ txt Pool_message.Field.(show Sessions |> CCString.capitalize_ascii)
          ]
      ; form
          ~a:
            [ a_method `Post
            ; a_action
                (enroll_contact_path
                   ~suffix:Experiment.(Id.value experiment.Experiment.id)
                   contact_id)
            ; a_class [ "stack" ]
            ]
          Input.
            [ csrf_element csrf ()
            ; session_select
            ; submit_element language Pool_message.Control.Enroll ()
            ]
      ]
  in
  let html = div ~a:[ a_class [ "stack-lg" ] ] [ notification; form ] in
  Modal.create ~active:true language title enroll_contact_modal_id html
;;

let assign_contact_experiment_list
  { Pool_context.language; _ }
  contact_id
  experiments
  =
  let base_class = [ "data-item" ] in
  let disabled =
    [ a_class ([ "bg-grey-light"; "not-allowed" ] @ base_class) ]
  in
  let htmx_attribs experiment_id =
    [ a_user_data "hx-trigger" "click"
    ; a_user_data
        "hx-get"
        (enroll_contact_path
           ~suffix:Experiment.(Id.value experiment_id)
           contact_id)
    ; a_user_data "hx-swap" "outerHTML"
    ; a_user_data "hx-target" (Format.asprintf "#%s" enroll_contact_modal_id)
    ]
  in
  let assignable_attrs experiment_id =
    a_class base_class :: htmx_attribs experiment_id
  in
  let not_matching_filter experiment_id =
    [ a_class ([ "bg-red-lighter" ] @ base_class) ] @ htmx_attribs experiment_id
  in
  div
    ~a:[ a_class [ "data-list"; "relative"; "flexcolumn"; "active" ] ]
    (match experiments with
     | [] ->
       [ div
           ~a:[ a_class [ "inset-sm"; "cursor-default" ] ]
           [ txt
               Pool_common.(Utils.text_to_string language I18n.EmptyListGeneric)
           ]
       ]
     | experiments ->
       let open Experiment in
       let open DirectEnrollment in
       CCList.map
         (fun ({ id; title; public_title; matches_filter; _ } as experiment) ->
           let attrs =
             if not (assignable experiment)
             then disabled
             else if not matches_filter
             then not_matching_filter id
             else assignable_attrs id
           in
           div
             ~a:attrs
             [ txt
                 (Format.asprintf
                    "%s (%s)"
                    (Title.value title)
                    (PublicTitle.value public_title))
             ])
         experiments)
;;

let assign_contact_form { Pool_context.csrf; language; _ } contact =
  let open Pool_common in
  let form_identifier = Pool_common.Id.(create () |> value) in
  let legend =
    Component.Table.(
      table_legend
        Pool_common.Utils.
          [ ( error_to_string
                language
                Pool_message.Error.ContactDoesNotMatchFilter
            , legend_color_item "bg-red-lighter" )
          ])
  in
  let form =
    match Contact.is_inactive contact with
    | true ->
      p
        [ txt
            Utils.(
              error_to_string language Pool_message.Error.ContactIsInactive)
        ]
    | false ->
      div
        [ div
            ~a:[ a_class [ "stack" ] ]
            [ legend
            ; form
                ~a:[ a_id form_identifier ]
                [ Input.csrf_element csrf ()
                ; Component.Search.Experiment.assign_contact_search
                    language
                    contact
                    ()
                ]
            ]
        ; div
            ~a:
              [ a_id enroll_contact_modal_id
              ; a_class [ "modal"; "fullscreen-overlay" ]
              ]
            []
        ]
  in
  div
    [ h3 [ txt Utils.(text_to_string language I18n.EnrollInExperiment) ]; form ]
;;

let list Pool_context.{ language; _ } contacts query =
  let url = Uri.of_string "/admin/contacts" in
  let data_table =
    Component.DataTable.create_meta
      ?filter:Contact.filterable_by
      ~search:Contact.searchable_by
      url
      query
      language
  in
  let cols = Pool_user.[ `column column_name; `column column_email; `empty ] in
  let th_class = [ "w-5"; "w-5"; "w-2" ] in
  let row (Contact.{ disabled; _ } as contact) =
    let a =
      if Pool_user.Disabled.value disabled
      then [ a_class [ "bg-red-lighter" ] ]
      else []
    in
    [ Status.identity_with_icons language true contact
    ; Status.email_with_icons language contact
    ; Input.link_as_button ~icon:Icon.Eye (path contact)
    ]
    |> CCList.map (CCList.return %> td)
    |> tr ~a
  in
  Component.DataTable.make
    ~target_id:"contacts-list"
    ~th_class
    ~cols
    ~row
    data_table
    contacts
;;

let index ({ Pool_context.language; _ } as context) contacts query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Contacts) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ Status.status_icons_table_legend language `All
        ; list context contacts query
        ]
    ]
;;

let experiment_history_modal_id = "past-experiment-modal"

let experiment_history Pool_context.{ language; _ } contact experiments query =
  let url =
    contact
    |> Contact.id
    |> Contact.Id.value
    |> Format.asprintf "/admin/contacts/%s/past-experiments"
    |> Uri.of_string
  in
  let data_table =
    DataTable.create_meta
      ?filter:Experiment.filterable_by
      ~push_url:false
      ~search:Experiment.searchable_by
      url
      query
      language
  in
  let cols =
    [ `column Experiment.column_title
    ; `column Experiment.column_public_title
    ; `empty
    ]
  in
  let th_class = [ "w-7"; "w-5" ] in
  let row (experiment, pending) =
    let open Experiment in
    let detail_btn = Page_admin_experiments.Partials.detail_button experiment in
    let session_modal_btn =
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
          ; hx_target ("#" ^ experiment_history_modal_id)
          ; a_class [ "primary" ]
          ]
      in
      button ~a:attributes [ Icon.(to_html InformationOutline) ]
    in
    let buttons =
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm"; "justify-end" ] ]
        [ session_modal_btn; detail_btn ]
    in
    let title =
      let text = txt (Title.value experiment.title) in
      match pending with
      | false -> text
      | true ->
        div
          ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
          [ span [ text ]
          ; span
              ~a:[ a_class [ "tag"; "primary"; "ghost"; "inline" ] ]
              [ txt "pending" ]
          ]
    in
    [ title; txt (PublicTitle.value experiment.public_title); buttons ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  DataTable.make
    ~target_id:"experiment-history"
    ~th_class
    ~cols
    ~row
    data_table
    experiments
;;

let experiment_history_modal
  { Pool_context.language; _ }
  (experiment : Experiment.t)
  assignments
  =
  let open Pool_common in
  let title (_ : Language.t) = Experiment.(Title.value experiment.title) in
  let html =
    let thead =
      Pool_message.Field.[ Start; NoShow; Participant ]
      |> CCList.map CCFun.(Utils.field_to_string_capitalized language %> txt)
    in
    let to_icon value =
      CCOption.map_or
        ~default:(txt "")
        CCFun.(value %> Component.Icon.bool_to_icon)
    in
    let rows =
      let open Assignment in
      assignments
      |> CCList.map (fun (session, { no_show; participated; _ }) ->
        let start = Session.start_end_with_duration_human session |> txt in
        let start =
          if CCOption.is_some session.Session.follow_up_to
          then div ~a:[ a_class [ "inset"; "left" ] ] [ start ]
          else start
        in
        [ start
        ; to_icon NoShow.value no_show
        ; to_icon Participated.value participated
        ])
    in
    Component.Table.horizontal_table ~thead `Striped rows
  in
  Modal.create ~active:true language title experiment_history_modal_id html
;;

let detail
  ?admin_comment
  (Pool_context.{ language; user; _ } as context)
  contact
  tags
  external_data_ids
  custom_fields
  past_experiments
  =
  let subtitle nav =
    h3
      ~a:[ a_class [ "heading-3" ] ]
      Pool_common.[ Utils.nav_link_to_string language nav |> txt ]
  in
  let buttons =
    let contact_path = path contact in
    let edit =
      Format.asprintf "%s/edit" contact_path
      |> Input.link_as_button
           ~icon:Icon.Create
           ~classnames:[ "small" ]
           ~control:(language, Pool_message.(Control.Edit (Some Field.Contact)))
    in
    let messages =
      let path =
        Format.asprintf "%s/messages" contact_path |> Sihl.Web.externalize_path
      in
      a
        ~a:[ a_class [ "btn small primary is-text has-icon" ]; a_href path ]
        [ Icon.(to_html Mail)
        ; txt
            Pool_common.(Utils.nav_link_to_string language I18n.MessageHistory)
        ]
    in
    div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] [ messages; edit ]
  in
  let past_experiments_html =
    let experiments, query = past_experiments in
    div
      [ div
          ~a:
            [ a_id experiment_history_modal_id
            ; a_class [ "modal"; "fullscreen-overlay" ]
            ]
          []
      ; h3
          [ txt
              Pool_common.(Utils.text_to_string language I18n.ExperimentHistory)
          ]
      ; experiment_history context contact experiments query
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
    [ div
        ~a:[ a_class [ "flexrow"; "wrap"; "flex-gap"; "justify-between" ] ]
        [ div [ heading_with_icons contact ]; buttons ]
    ; personal_detail ?admin_comment ~custom_fields ~tags user language contact
    ; div
        [ subtitle Pool_common.I18n.ExternalDataIds
        ; Component.Contacts.external_data_ids language external_data_ids
        ]
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ assign_contact_form context contact ]
    ; past_experiments_html
    ]
;;

let tag_form
  (Pool_context.{ query_language; _ } as context)
  ?existing
  available
  contact
  =
  let action =
    Http_utils.externalize_path_with_lang
      query_language
      (Format.asprintf
         "%s/%s/assign"
         (contact |> path)
         Pool_message.Field.(Tag |> human_url))
  in
  Component.Tag.add_tags_form ?existing context available action
;;

let promote_form csrf language query_language contact =
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/contacts/%s/promote"
      Contact.(contact |> id |> Id.value)
    |> Http_utils.externalize_path_with_lang query_language
  in
  [ div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
      [ form
          ~a:
            [ a_method `Post
            ; a_action action
            ; a_user_data
                "confirmable"
                (Utils.confirmable_to_string language I18n.PromoteContact)
            ]
          [ Input.csrf_element csrf ()
          ; Input.submit_element
              ~submit_type:`Success
              ~classnames:[ "nobr" ]
              language
              Pool_message.Control.PromoteContact
              ()
          ]
      ; div
          ~a:[ a_class [ "grow" ] ]
          [ txt Pool_common.(Utils.hint_to_string language I18n.PromoteContact)
          ]
      ]
  ]
;;

let edit
  ?(allowed_to_assign = false)
  ?(allowed_to_promote = false)
  (Pool_context.{ language; csrf; query_language; _ } as context)
  tenant_languages
  contact
  custom_fields
  tags
  available_tags
  =
  let base_action = Htmx.admin_profile_hx_post (Contact.id contact) in
  let assign_tags =
    if allowed_to_assign
    then (
      let remove_action tag =
        Format.asprintf
          "%s/%s/%s/remove"
          base_action
          Pool_message.Field.(Tag |> human_url)
          Tags.(Id.value tag.id)
      in
      div
        [ h2
            ~a:[ a_class [ "heading-2" ] ]
            Pool_common.[ Utils.nav_link_to_string language I18n.Tags |> txt ]
        ; div
            ~a:[ a_class [ "switcher-lg"; "flex-gap" ] ]
            [ tag_form context ~existing:tags available_tags contact
            ; Component.Tag.tag_form
                ~label:Pool_common.I18n.SelectedTags
                language
                (remove_action, csrf)
                tags
            ]
        ])
    else txt ""
  in
  let promote_form =
    if allowed_to_promote
    then promote_form csrf language query_language contact
    else []
  in
  let form_context = `Admin in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ heading_with_icons contact
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ Page_contact_edit.personal_details_form
            csrf
            language
            query_language
            form_context
            tenant_languages
            contact
            custom_fields
        ; assign_tags
        ; Page_contact_edit.status_form
            ~additional:promote_form
            csrf
            language
            query_language
            contact
            form_context
        ; p
            [ a
                ~a:
                  [ a_href
                      (Sihl.Web.externalize_path
                         (Format.asprintf
                            "/admin/contacts/%s"
                            Contact.(contact |> id |> Id.value)))
                  ]
                [ Pool_message.Control.Back
                  |> Pool_common.Utils.control_to_string language
                  |> txt
                ]
            ]
        ]
    ]
;;

let external_data_ids { Pool_context.language; _ } contact external_data_ids =
  let table = Component.Contacts.external_data_ids language external_data_ids in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack" ] ]
    [ h1 [ txt (Contact.fullname contact) ]
    ; h2
        [ txt
            Pool_common.(Utils.nav_link_to_string language I18n.ExternalDataIds)
        ]
    ; table
    ]
;;

let message_history_url contact =
  Uri.of_string
    (Format.asprintf
       "/admin/contacts/%s/messages"
       Contact.(contact |> id |> Id.value))
;;

let message_history ({ Pool_context.language; _ } as context) contact messages =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.text_to_string
                language
                I18n.(MessageHistory (Contact.lastname_firstname contact)))
        ]
    ; Page_admin_queue_mapping.list
        context
        (message_history_url contact)
        messages
    ]
;;
