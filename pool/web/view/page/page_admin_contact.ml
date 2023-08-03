open CCFun
open Tyxml.Html
open Component

let path =
  Contact.id %> Pool_common.Id.value %> Format.asprintf "/admin/contacts/%s"
;;

let personal_detail ?(admin_comment = None) language contact =
  let open Contact in
  let open Pool_common.Message in
  let with_comment =
    match admin_comment with
    | None -> []
    | Some comment ->
      [ ( Field.AdminComment
        , comment |> AdminComment.value |> Http_utils.add_line_breaks )
      ]
  in
  Pool_common.Message.(
    [ Field.Name, fullname contact |> txt
    ; Field.Email, email_address contact |> Pool_user.EmailAddress.value |> txt
    ; ( Field.CellPhone
      , contact.cell_phone
        |> CCOption.map_or ~default:"" Pool_user.CellPhone.value
        |> txt )
    ]
    @ with_comment)
  |> Table.vertical_table `Striped language
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

let contact_overview language contacts =
  let open Contact in
  let open Pool_user in
  let thead =
    (Pool_common.Message.Field.[ Email; Name; Status ]
     |> Table.fields_to_txt language)
    @ [ txt "" ]
  in
  let status_icons { paused; terms_accepted_at; import_pending; _ } =
    let open Icon in
    let success = to_html ~classnames:[ "color-green" ] in
    let error = to_html ~classnames:[ "color-red" ] in
    let paused =
      match paused |> Paused.value with
      | true -> error NotificationsOff
      | false -> success Notifications
    in
    [ terms_accepted_at |> CCOption.is_some, Checkmark
    ; import_pending |> ImportPending.value, ChevronForwardCircle
    ]
    |> CCList.map (fun (status, icon) ->
      if status then success icon else error icon)
    |> CCList.cons paused
    |> div ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
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
          [ txt (email_address contact |> EmailAddress.value)
          ; txt (fullname contact)
          ; status_icons contact
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
  let table_legend =
    let open Pool_common in
    let open Component.Table in
    let open Icon in
    let field_to_string m =
      m |> Utils.field_to_string language |> CCString.capitalize_ascii
    in
    let text_to_string m = m |> Utils.text_to_string language in
    table_legend
      [ text_to_string I18n.Disabled, legend_color_item "bg-red-lighter"
      ; field_to_string Message.Field.Paused, legend_icon_item Notifications
      ; field_to_string Message.Field.TermsAccepted, legend_icon_item Checkmark
      ; ( field_to_string Message.Field.ImportPending
        , legend_icon_item ChevronForwardCircle )
      ]
  in
  List.create
    ~legend:table_legend
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

let detail ?admin_comment Pool_context.{ language; _ } contact tags =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
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
    ; div
        ~a:
          [ a_class
              [ "flexrow"
              ; "flexcolumn-reversed-tablet"
              ; "flex-gap"
              ; "gap-lg"
              ; "reverse"
              ]
          ]
        [ personal_detail ?admin_comment language contact
        ; div
            [ h3
                ~a:[ a_class [ "heading-3" ] ]
                Pool_common.
                  [ Utils.nav_link_to_string language I18n.Tags |> txt ]
            ; Component.Tag.tag_list language tags
            ]
        ]
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
            ; Component.Tag.tag_list
                language
                ~remove_action:(remove_action, csrf)
                ~title:Pool_common.I18n.SelectedTags
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
