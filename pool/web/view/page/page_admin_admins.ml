open CCFun
open Tyxml.Html
open Component
open Pool_message
module Status = UserStatus.Admin

let admin_path = Http_utils.Url.Admin.admin_path

module List = struct
  open Admin

  let cols ~hide_create language =
    let create : [ | Html_types.flow5 ] elt =
      Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Control.Add (Some Field.Admin))
        (admin_path ~suffix:"new" ())
    in
    Pool_user.
      [ `column column_name
      ; `column column_email
      ; (if hide_create then `empty else `custom create)
      ]
  ;;

  let row ?(additional_buttons = []) language admin =
    let detail_button =
      admin_path ~id:(id admin) () |> Input.link_as_button ~icon:Icon.Eye
    in
    let buttons =
      div
        ~a:[ a_class [ "flexrow"; "flex-gap-xs"; "justify-end" ] ]
        (detail_button :: additional_buttons)
    in
    [ txt (fullname_reversed admin), Some Field.Name
    ; Status.email_with_icons admin, Some Field.Email
    ; buttons, None
    ]
    |> CCList.map (fun (html, field) ->
      let label = Table.data_label_opt language field in
      td ~a:label [ html ])
    |> tr
  ;;
end

let list
      ?(table_id = "admin-list")
      ?(hide_create = false)
      ?(url = admin_path ())
      ?push_url
      Pool_context.{ language; guardian; _ }
      (admins, query)
  =
  let open Guard in
  let can_add_admin =
    PermissionOnTarget.(validate (create Permission.Create `Admin) guardian)
  in
  let url = Uri.of_string url in
  let data_table =
    DataTable.create_meta ~search:Contact.searchable_by ?push_url url query language
  in
  let cols = List.cols ~hide_create:((not can_add_admin) || hide_create) language in
  let th_class = [ "w-5"; "w-5"; "w-2" ] in
  let row = List.row language in
  DataTable.make
    ~break_mobile:true
    ~th_class
    ~target_id:table_id
    ~cols
    ~row
    data_table
    admins
;;

let static_overview ?(disable_edit = false) language admins =
  let thead =
    let add_admin =
      Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Control.Add (Some Field.Admin))
        "/admin/admins/new"
    in
    let to_txt = Table.field_to_txt language in
    let base = Field.[ Email |> to_txt; Name |> to_txt ] in
    match disable_edit with
    | false -> base @ [ add_admin ]
    | true -> base
  in
  CCList.map
    (fun admin ->
       let user = Admin.user admin in
       let base = [ Status.email_with_icons admin; txt (Pool_user.fullname user) ] in
       match disable_edit with
       | false ->
         base
         @ [ Format.asprintf "/admin/admins/%s" (user.Pool_user.id |> Pool_user.Id.value)
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

let roles_list ?is_edit ?top_element ({ Pool_context.language; _ } as context) target_id =
  let open Component.Role in
  List.create ?is_edit ~path:"/admin/admins" context target_id
  %> CCList.return
  %> roles_section ?top_element language
;;

let new_form { Pool_context.language; csrf; _ } =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        Pool_common.
          [ Control.(Create (Some Field.Admin)) |> Utils.control_to_string language |> txt
          ]
    ; form
        ~a:
          [ a_action (Sihl.Web.externalize_path "/admin/admins")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        ([ Input.csrf_element csrf ()
         ; div
             ~a:[ a_class [ "grid-col-2"; "flex-gap" ] ]
             (CCList.map
                (fun (field, input) ->
                   Input.input_element ~required:true language input field)
                Field.
                  [ Email, `Email
                  ; Password, `Password
                  ; Firstname, `Text
                  ; Lastname, `Text
                  ])
         ]
         @ [ div
               ~a:[ a_class [ "flexrow" ] ]
               [ Input.submit_element
                   ~classnames:[ "push" ]
                   language
                   Control.(Create (Some Field.admin))
                   ()
               ]
           ])
    ]
;;

let index (Pool_context.{ language; _ } as context) admins =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Admins) ]
    ; list context admins
    ]
;;

let detail
      ({ Pool_context.language; guardian; _ } as context)
      admin
      target_id
      granted_roles
      failed_login_attempt
  =
  let user = Admin.user admin in
  let failed_login_attempt =
    let can_unblock =
      Guard.PermissionOnTarget.validate
        Admin.(Guard.Access.can_update_target (id admin))
        guardian
    in
    let unblock_url =
      if can_unblock
      then
        Some (Http_utils.Url.Admin.admin_path ~suffix:"unblock" ~id:(Admin.id admin) ())
      else None
    in
    Component.User.account_suspension_notification
      ?unblock_url
      context
      failed_login_attempt
  in
  [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt (Pool_user.fullname user) ]
  ; failed_login_attempt
  ; Input.link_as_button
      ~icon:Icon.Create
      ~control:(language, Control.(Edit None))
      (Format.asprintf "/admin/admins/%s/edit" (user.Pool_user.id |> Pool_user.Id.value))
  ; roles_list context target_id granted_roles
  ]
  |> div ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
;;

let edit context editable_admin target_id granted_roles top_element =
  let user = Admin.user editable_admin in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt (Pool_user.fullname user) ]
    ; roles_list ~is_edit:true ~top_element context target_id granted_roles
    ]
;;
