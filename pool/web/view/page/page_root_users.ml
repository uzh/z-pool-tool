open Tyxml.Html
open Component.Input
module Table = Component.Table
module Message = Pool_common.Message

let list root_list Pool_context.{ language; csrf; _ } =
  let build_root_rows root_list =
    let open Sihl.Contract.User in
    let status_toggle (status : Sihl.Contract.User.status) id =
      let text, style =
        match status with
        | Active -> Message.Disable, "error"
        | Inactive -> Message.Enable, "primary"
      in
      form
        ~a:
          [ a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf "/root/users/%s/toggle-status" id))
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ submit_element language text ~classnames:[ style ] () ]
    in
    let thead =
      Pool_common.Message.[ Field.Email |> Table.field_to_txt language; txt "" ]
    in
    let rows =
      CCList.map
        (fun root ->
          let user = root |> Admin.user in
          let status = status_toggle user.status user.id in
          [ txt user.email; status ])
        root_list
    in
    Component.Table.horizontal_table `Striped rows ~align_last_end:true ~thead
  in
  let root_list = build_root_rows root_list in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Users) ]
    ; root_list
    ; form
        ~a:
          [ a_action (Sihl.Web.externalize_path "/root/users/create")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        (CCList.map
           (input_element language `Text)
           Message.Field.[ Email; Password; Firstname; Lastname ]
        @ [ csrf_element csrf ()
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.(Create (Some Field.root))
                  ()
              ]
          ])
    ]
;;
