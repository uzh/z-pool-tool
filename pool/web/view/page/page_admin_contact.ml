open CCFun
open Tyxml.Html
module Table = Component.Table
module Input = Component.Input
module Icon = Component.Icon
module Modal = Component.Modal

let path =
  Contact.id %> Pool_common.Id.value %> Format.asprintf "/admin/contacts/%s"
;;

let enroll_contact_modal_id = "enroll-modal"

let enroll_contact_path ?suffix contact_id =
  let open Pool_common in
  Format.asprintf
    "/admin/contacts/%s/%s"
    (Contact.Id.value contact_id)
    Message.Field.(Experiments |> human_url)
  |> (fun base ->
       match suffix with
       | None -> base
       | Some suffix -> Format.asprintf "%s/%s" base suffix)
  |> Sihl.Web.externalize_path
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
  let open Pool_common.Message in
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
        ; td [ Component.CustomField.answer current_user language custom_field ]
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
  Pool_common.Message.(
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
  table
    ~a:[ a_class (Component.Table.table_classes `Striped ~align_top:true ()) ]
    rows
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
  experiment
  sessions
  matches_filter
  =
  let open Pool_common in
  let title language = Utils.text_to_string language I18n.EnrollInExperiment in
  let notification =
    match matches_filter with
    | true -> txt ""
    | false ->
      Component.Notification.notification
        language
        `Error
        [ p
            Utils.
              [ txt (hint_to_string language I18n.ContactDoesNotMatchFilter) ]
        ]
  in
  let session_select =
    let label = Session.start_end_to_human in
    let follow_up_row session = div [ txt (label session) ] in
    let row (session, follow_ups) =
      let tooltip, attribs =
        Session.assignment_creatable session
        |> function
        | Ok () ->
          ( txt ""
          , [ a_name Message.Field.(show Session)
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
      [ h4 [ txt Message.Field.(show Sessions |> CCString.capitalize_ascii) ]
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
            ; submit_element language Message.Enroll ()
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
  let open Experiment in
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
  let assignable experiment_id =
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
       CCList.map
         (fun DirectEnrollment.(
                { id; title; public_title; matches_filter; _ } as experiment) ->
           let attrs =
             if not (DirectEnrollment.assignable experiment)
             then disabled
             else if not matches_filter
             then not_matching_filter id
             else assignable id
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
          [ ( error_to_string language Message.ContactDoesNotMatchFilter
            , legend_color_item "bg-red-lighter" )
          ])
  in
  div
    [ h3
        [ txt
            Pool_common.Utils.(text_to_string language I18n.EnrollInExperiment)
        ]
    ; div
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
;;

let contact_overview language contacts =
  let open Contact in
  let open Pool_user in
  let thead =
    (Pool_common.Message.Field.[ Name; Email ] |> Table.fields_to_txt language)
    @ [ txt "" ]
  in
  let user_table contacts =
    let rows =
      CCList.map
        (fun contact ->
          let row =
            match contact.disabled |> Disabled.value with
            | true -> tr ~a:[ a_class [ "bg-red-lighter" ] ]
            | false -> tr ~a:[]
          in
          [ Component.Contacts.identity_with_icons
              true
              contact
              Contact.(id contact)
          ; txt (email_address contact |> EmailAddress.value)
          ; contact |> path |> Input.link_as_button ~icon:Icon.Eye
          ]
          |> CCList.map (fun cell -> td [ cell ])
          |> row)
        contacts
    in
    let thead = Table.table_head thead in
    table
      ~thead
      ~a:[ a_class (Table.table_classes `Striped ~align_last_end:true ()) ]
      rows
  in
  Component.List.create
    ~legend:
      (Component.Contacts.status_icons_table_legend language
       |> Component.Table.table_legend)
    language
    user_table
    Contact.sortable_by
    Contact.searchable_by
    contacts
;;

let index Pool_context.{ language; _ } contacts =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Contacts) ]
    ; contact_overview language contacts
    ]
;;

let detail
  ?admin_comment
  (Pool_context.{ language; user; _ } as context)
  contact
  tags
  external_data_ids
  custom_fields
  =
  let subtitle nav =
    h3
      ~a:[ a_class [ "heading-3" ] ]
      Pool_common.[ Utils.nav_link_to_string language nav |> txt ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
    [ div
        ~a:[ a_class [ "flexrow"; "wrap"; "flex-gap"; "justify-between" ] ]
        [ div
            [ h1
                ~a:[ a_class [ "heading-1" ] ]
                [ txt (Contact.fullname contact) ]
            ]
        ; contact
          |> path
          |> Format.asprintf "%s/edit"
          |> Input.link_as_button
               ~icon:Icon.Create
               ~classnames:[ "small" ]
               ~control:
                 Pool_common.(language, Message.(Edit (Some Field.Contact)))
        ]
    ; personal_detail ?admin_comment ~custom_fields ~tags user language contact
    ; div
        [ subtitle Pool_common.I18n.ExternalDataIds
        ; Component.Contacts.external_data_ids language external_data_ids
        ]
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ assign_contact_form context contact ]
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
         Pool_common.Message.Field.(Tag |> human_url))
  in
  Component.Tag.add_tags_form ?existing context available action
;;

let promote_form csrf language query_language contact =
  let open Pool_common in
  let action =
    Format.asprintf
      "/admin/contacts/%s/promote"
      (contact |> Contact.id |> Pool_common.Id.value)
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
              Message.PromoteContact
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
          Pool_common.Message.Field.(Tag |> human_url)
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
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (Contact.fullname contact) ]
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
                            (contact |> Contact.id |> Pool_common.Id.value)))
                  ]
                [ txt
                    Pool_common.(
                      Message.Back |> Utils.control_to_string language)
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
