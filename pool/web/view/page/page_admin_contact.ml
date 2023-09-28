open CCFun
open Tyxml.Html
module Table = Component.Table
module Input = Component.Input
module Icon = Component.Icon

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
  |> Table.vertical_table ~align_top:true `Striped language
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
  Pool_context.{ language; _ }
  contact
  tags
  external_data_ids
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
            [ subtitle Pool_common.I18n.Tags
            ; Component.Tag.tag_list language tags
            ]
        ]
    ; div
        [ subtitle Pool_common.I18n.ExternalDataIds
        ; Component.Contacts.external_data_ids language external_data_ids
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
