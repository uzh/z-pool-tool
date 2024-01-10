open CCFun
open Tyxml.Html
open Component
module Status = UserStatus.Admin

let list Pool_context.{ language; _ } (admins, query) =
  let open Admin in
  let url = Uri.of_string "/admin/admins" in
  let sort =
    Component.DataTable.
      { url; query; language; search = Some Contact.searchable_by }
  in
  let cols = Pool_user.[ `column column_name; `column column_email; `empty ] in
  let th_class = [ "w-5"; "w-5"; "w-2" ] in
  let row admin =
    let button =
      admin
      |> id
      |> Id.value
      |> Format.asprintf "/admin/admins/%s"
      |> Input.link_as_button ~icon:Icon.Eye
    in
    [ txt (full_name_reversed admin); Status.email_with_icons admin; button ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~th_class
    ~target_id:"admin-list"
    ~cols
    ~row
    sort
    admins
;;

let static_overview ?(disable_edit = false) language admins =
  let thead =
    let add_admin =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Pool_common.Message.(Add (Some Field.Admin)))
        "/admin/admins/new"
    in
    let to_txt = Component.Table.field_to_txt language in
    let base = Pool_common.Message.Field.[ Email |> to_txt; Name |> to_txt ] in
    match disable_edit with
    | false -> base @ [ add_admin ]
    | true -> base
  in
  CCList.map
    (fun admin ->
      let open Sihl_user in
      let default_empty o = CCOption.value ~default:"" o in
      let user = Admin.user admin in
      let base =
        [ Status.email_with_icons admin
        ; txt
            (Format.asprintf
               "%s %s"
               (user.given_name |> default_empty)
               (user.name |> default_empty)
             |> CCString.trim)
        ]
      in
      match disable_edit with
      | false ->
        base
        @ [ Format.asprintf "/admin/admins/%s" user.id
            |> Input.link_as_button ~icon:Icon.Eye
          ]
      | true -> base)
    admins
  |> fun rows ->
  div
    ~a:[ a_class [ "stack" ] ]
    [ Status.status_icons_table_legend language
    ; Table.horizontal_table ~align_last_end:true `Striped ~thead rows
    ]
;;

let roles_section ?(top_element = []) language children =
  let open Pool_common in
  h2
    ~a:[ a_class [ "heading-2" ] ]
    [ Utils.text_to_string language I18n.RolesGranted |> txt ]
  :: (top_element @ children)
;;

let roles_list
  ?is_edit
  ?top_element
  ({ Pool_context.language; _ } as context)
  admin
  =
  Component.Role.List.create ?is_edit context admin
  %> CCList.return
  %> roles_section ?top_element language
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

let index (Pool_context.{ language; _ } as context) admins =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Admins) ]
    ; list context admins
    ]
;;

let detail ({ Pool_context.language; _ } as context) admin granted_roles =
  let open Sihl.Contract.User in
  let open Pool_common in
  let user = Admin.user admin in
  [ h1
      ~a:[ a_class [ "heading-1" ] ]
      [ txt
          (Format.asprintf
             "%s %s"
             (user.given_name |> Option.value ~default:"")
             (user.name |> Option.value ~default:""))
      ]
  ; Input.link_as_button
      ~icon:Icon.Create
      ~control:(language, Message.(Edit None))
      (Format.asprintf "/admin/admins/%s/edit" user.id)
  ]
  @ roles_list context admin granted_roles
  |> div ~a:[ a_class [ "trim"; "safety-margin" ] ]
;;

let edit context editabe_admin granted_roles top_element =
  let open Sihl.Contract.User in
  let user = Admin.user editabe_admin in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    ([ h1
         ~a:[ a_class [ "heading-1" ] ]
         [ txt
             (Format.asprintf
                "%s %s"
                (user.given_name |> Option.value ~default:"")
                (user.name |> Option.value ~default:""))
         ]
     ]
     @ roles_list ~is_edit:true ~top_element context editabe_admin granted_roles
    )
;;
