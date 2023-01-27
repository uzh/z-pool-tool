open Tyxml.Html

let personal_detail language contact =
  let open Contact in
  Pool_common.Message.
    [ Field.Name, fullname contact |> txt
    ; Field.Email, email_address contact |> Pool_user.EmailAddress.value |> txt
    ]
  |> Component.Table.vertical_table `Striped language
;;

let contact_overview language contacts =
  let open Contact in
  let thead =
    (Pool_common.Message.Field.[ Email; Name ]
    |> Component.Table.fields_to_txt language)
    @ [ txt "" ]
  in
  CCList.map
    (fun contact ->
      [ txt (email_address contact |> Pool_user.EmailAddress.value)
      ; txt (fullname contact)
      ; id contact
        |> Pool_common.Id.value
        |> Format.asprintf "/admin/contacts/%s"
        |> Component.Input.edit_link
      ])
    contacts
  |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
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

let detail Pool_context.{ language; _ } contact =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ div
        ~a:
          [ a_class
              [ "flexrow"; "justify-between"; "flex-gap"; "flexcolumn-mobile" ]
          ]
        [ div
            [ h1
                ~a:[ a_class [ "heading-1" ] ]
                [ txt (Contact.fullname contact) ]
            ]
        ; contact
          |> Contact.id
          |> Pool_common.Id.value
          |> Format.asprintf "/admin/contacts/%s/edit"
          |> Component.Input.link_as_button
               ~icon:`Create
               ~classnames:[ "small" ]
               ~control:
                 Pool_common.(language, Message.(Edit (Some Field.Contact)))
        ]
    ; div ~a:[ a_class [ "gap-lg" ] ] [ personal_detail language contact ]
    ]
;;

let edit
  Pool_context.{ language; csrf; query_language; user; _ }
  tenant_languages
  contact
  custom_fields
  =
  let is_admin = Pool_context.user_is_admin user in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (Contact.fullname contact) ]
    ; Page_contact_edit.personal_details_form
        csrf
        language
        query_language
        (Htmx.admin_profile_hx_post (Contact.id contact))
        tenant_languages
        contact
        custom_fields
        is_admin
    ; p
        [ a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/admin/contacts/%s"
                        (contact |> Contact.id |> Pool_common.Id.value)))
              ]
            [ txt Pool_common.(Message.Back |> Utils.control_to_string language)
            ]
        ]
    ]
;;
