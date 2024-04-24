open Tyxml.Html
open Component.Input
open Pool_common
module Table = Component.Table

let table Pool_context.{ language; _ } (admins, query) =
  let open Admin in
  let url = Uri.of_string "/admin/admins" in
  let data_table =
    Component.DataTable.create_meta
      ~search:Contact.searchable_by
      url
      query
      language
  in
  let cols = Pool_user.[ `column column_name; `column column_email; `empty ] in
  let row admin =
    let open Pool_user in
    let status_toggle =
      let user = admin.user in
      let text, style =
        match user.status with
        | Status.Active -> Pool_message.Control.Disable, "error"
        | Status.Inactive -> Pool_message.Control.Enable, "primary"
      in
      form
        ~a:
          [ a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf
                    "/root/users/%s/toggle-status"
                    (user.Pool_user.id |> Pool_user.Id.value)))
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ submit_element language text ~classnames:[ style ] () ]
    in
    [ txt (Admin.email_address admin |> Pool_user.EmailAddress.value)
    ; txt (full_name admin)
    ; status_toggle
    ]
    |> CCList.map CCFun.(CCList.return %> td)
    |> tr
  in
  Component.DataTable.make ~target_id:"admin-list" ~cols ~row data_table admins
;;

let list root_list (Pool_context.{ language; csrf; _ } as context) flash_fetcher
  =
  let open Pool_message in
  let root_list = table context root_list in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.nav_link_to_string language I18n.Users) ]
    ; root_list
    ; h2
        ~a:[ a_class [ "heading-2" ] ]
        [ Utils.control_to_string language Control.(Create (Some Field.Root))
          |> txt
        ]
    ; form
        ~a:
          [ a_action (Sihl.Web.externalize_path "/root/users/create")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ csrf_element csrf ()
        ; input_element ~flash_fetcher language `Text Field.Email
        ; input_element language `Password Field.Password
        ; input_element ~flash_fetcher language `Text Field.Firstname
        ; input_element ~flash_fetcher language `Text Field.Lastname
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element
                ~classnames:[ "push" ]
                language
                Control.(Create None)
                ()
            ]
        ]
    ]
;;
