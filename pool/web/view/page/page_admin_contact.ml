open CCFun
open Tyxml.Html
open Component

let path =
  Contact.id %> Pool_common.Id.value %> Format.asprintf "/admin/contacts/%s"
;;

let personal_detail language contact =
  let open Contact in
  Pool_common.Message.
    [ Field.Name, fullname contact |> txt
    ; Field.Email, email_address contact |> Pool_user.EmailAddress.value |> txt
    ; ( Field.CellPhone
      , contact.cell_phone
        |> CCOption.map_or ~default:"" Pool_user.CellPhone.value
        |> txt )
    ]
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
  let thead =
    (Pool_common.Message.Field.[ Email; Name ] |> Table.fields_to_txt language)
    @ [ txt "" ]
  in
  let user_table contacts =
    let rows =
      CCList.map
        (fun contact ->
          [ txt (email_address contact |> Pool_user.EmailAddress.value)
          ; txt (fullname contact)
          ; contact |> path |> Input.edit_link
          ])
        contacts
    in
    rows |> Table.horizontal_table `Striped ~align_last_end:true ~thead
  in
  List.create
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

let detail Pool_context.{ language; _ } contact tags =
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
        [ personal_detail language contact
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

let edit
  ?(allowed_to_assign = false)
  (Pool_context.{ language; csrf; query_language; user; _ } as context)
  tenant_languages
  contact
  custom_fields
  tags
  available_tags
  =
  let is_admin = Pool_context.user_is_admin user in
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
    else []
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    ([ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (Contact.fullname contact) ]
     ; Page_contact_edit.personal_details_form
         csrf
         language
         query_language
         base_action
         tenant_languages
         contact
         custom_fields
         is_admin
     ]
     @ assign_tags
     @ [ p
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
       ])
;;
