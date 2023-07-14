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
        ~a:
          [ a_class
              [ "flexrow"; "justify-between"; "flex-gap"; "flexcolumn-mobile" ]
          ]
        ((div
            [ h1
                ~a:[ a_class [ "heading-1" ] ]
                [ txt (Contact.fullname contact) ]
            ]
          :: CCList.map
               (fun tag ->
                 div
                   ~a:[ a_class [ "tag"; "primary" ] ]
                   Tags.[ tag.title |> Title.value |> txt ])
               tags)
         @ [ contact
             |> path
             |> Format.asprintf "%s/edit"
             |> Input.link_as_button
                  ~icon:Icon.Create
                  ~classnames:[ "small" ]
                  ~control:
                    Pool_common.(language, Message.(Edit (Some Field.Contact)))
           ])
    ; div ~a:[ a_class [ "gap-lg" ] ] [ personal_detail language contact ]
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
      (contact |> path |> Format.asprintf "%s/assign-tag")
  in
  let available =
    CCList.(filter (flip (mem ~eq:Tags.equal) existing %> not) available)
  in
  form
    ~a:[ a_method `Post; a_action action; a_class [ "stack" ] ]
    Input.
      [ csrf_element csrf ()
      ; selector
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
  let assign_tags =
    if allowed_to_assign
    then [ tag_form context ~existing:tags available_tags contact ]
    else []
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    ([ h1 ~a:[ a_class [ "heading-1" ] ] [ txt (Contact.fullname contact) ]
     ; Page_contact_edit.personal_details_form
         csrf
         language
         query_language
         (Htmx.admin_profile_hx_post (Contact.id contact))
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
