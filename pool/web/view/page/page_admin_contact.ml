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
            ; Component.Tag.tag_list tags
            ]
        ]
    ]
;;

let tag_form
  Pool_context.{ language; csrf; query_language; _ }
  ?(existing = [])
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
  let available =
    CCList.(filter (flip (mem ~eq:Tags.equal) existing %> not) available)
  in
  form
    ~a:[ a_method `Post; a_action action ]
    Input.
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ selector
              ~add_empty:true
              ~option_formatter:Tags.(fun tag -> Title.value tag.title)
              language
              Pool_common.Message.Field.Tag
              Tags.(fun tag -> Id.value tag.id)
              available
              None
              ()
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Pool_common.Message.(Add (Some Field.Tag))
                  ()
              ]
          ]
      ]
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
          ; div
              ~a:[ a_class [ "form-group" ] ]
              [ label
                  Pool_common.
                    [ Utils.control_to_string
                        language
                        Message.(Remove (Some Field.Tag))
                      |> txt
                    ]
              ; Component.Tag.tag_list
                  ~remove_action:(remove_action, csrf, language)
                  tags
              ]
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
