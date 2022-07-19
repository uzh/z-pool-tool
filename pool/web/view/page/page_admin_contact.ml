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
  let thead = Pool_common.Message.Field.[ Some Email; Some Name; None ] in
  CCList.map
    (fun contact ->
      [ txt (email_address contact |> Pool_user.EmailAddress.value)
      ; txt (fullname contact)
      ; a
          ~a:
            [ a_href
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/admin/contacts/%s"
                      (id contact |> Pool_common.Id.value)))
            ]
          [ txt Pool_common.(Utils.control_to_string language Message.More) ]
      ])
    contacts
  |> Component.Table.horizontal_table `Striped ~thead language
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
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (Contact.fullname contact) ]
    ; personal_detail language contact
    ; p
        [ a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/admin/contacts/%s/edit"
                        (contact |> Contact.id |> Pool_common.Id.value)))
              ]
            [ txt
                Pool_common.(
                  Message.(Edit None) |> Utils.control_to_string language)
            ]
        ]
    ]
;;

let edit
    Pool_context.{ language; csrf; query_language; _ }
    user_update_csrf
    tenant_languages
    contact
  =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (Contact.fullname contact) ]
    ; Page_contact_edit.personal_details_form
        csrf
        user_update_csrf
        language
        query_language
        (Format.asprintf
           "/admin/contacts/%s"
           (Contact.id contact |> Pool_common.Id.value))
        tenant_languages
        contact
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
