open Tyxml.Html
open Component

let admin_overview language admins =
  let thead =
    let add_admin =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:`Add
        ~control:(language, Pool_common.Message.(Add (Some Field.Admin)))
        "/admin/admins/new"
    in
    let to_txt = Component.Table.field_to_txt language in
    Pool_common.Message.Field.[ Email |> to_txt; Name |> to_txt; add_admin ]
  in
  CCList.map
    (fun admin ->
      let open Sihl_user in
      let default_empty o = CCOption.value ~default:"" o in
      let user = Admin.user admin in
      [ txt user.email
      ; txt
          (Format.asprintf
             "%s %s"
             (user.given_name |> default_empty)
             (user.name |> default_empty)
           |> CCString.trim)
      ; Format.asprintf "/admin/admins/%s" user.id |> Input.edit_link
      ])
    admins
  |> Table.horizontal_table ~align_last_end:true `Striped ~thead
;;

let new_form { Pool_context.language; csrf; _ } =
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        Pool_common.
          [ Message.(create (Some Field.Admin))
            |> Utils.control_to_string language
            |> txt
          ]
    ; form
        ~a:
          [ a_action (Sihl.Web.externalize_path "/admin/admins")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        ((Input.csrf_element csrf ()
          :: CCList.map
               (fun (field, input) ->
                 Input.input_element ~required:true language input field)
               Pool_common.Message.Field.
                 [ Email, `Email
                 ; Password, `Password
                 ; Firstname, `Text
                 ; Lastname, `Text
                 ])
         @ [ div
               ~a:[ a_class [ "flexrow" ] ]
               [ Input.submit_element
                   ~classnames:[ "push" ]
                   language
                   Pool_common.Message.(Create (Some Field.admin))
                   ()
               ]
           ])
    ]
;;

let index Pool_context.{ language; _ } admins =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Admins) ]
    ; admin_overview language admins
    ]
;;

let detail Pool_context.{ language; _ } admin =
  let open Sihl.Contract.User in
  let user = Admin.user admin in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Format.asprintf
               "%s %s"
               (user.given_name |> Option.value ~default:"")
               (user.name |> Option.value ~default:""))
        ]
    ; p
        [ a
            ~a:
              [ a_href
                  (Format.asprintf "/admin/admins/%s/edit" user.id
                   |> Sihl.Web.externalize_path)
              ]
            [ txt
                Pool_common.(
                  Utils.control_to_string language Message.(Edit None))
            ]
        ]
    ]
;;

let edit _ editabe_admin =
  let open Sihl.Contract.User in
  let user = Admin.user editabe_admin in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Format.asprintf
               "%s %s"
               (user.given_name |> Option.value ~default:"")
               (user.name |> Option.value ~default:""))
        ]
    ]
;;
