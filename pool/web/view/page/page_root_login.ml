open Tyxml.Html
open Component
module Message = Pool_common.Message

let login csrf message Pool_context.{ language; _ } =
  let input_element = input_element language in
  let html =
    div
      [ h1 [ txt "Root Login" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/login")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") Message.Email ""
          ; input_element `Password (Some "password") Message.Password ""
          ; submit_element
              language
              Pool_common.Message.(Login)
              ~classnames:[ "button--primary" ]
              ()
          ]
      ; a
          ~a:
            [ a_href (Sihl.Web.externalize_path "/root/request-reset-password")
            ]
          [ txt "Reset password" ]
      ]
  in
  Page_layout.create_root_layout html message language
;;

let request_reset_password csrf message Pool_context.{ language; _ } =
  let input_element = input_element language in
  let html =
    div
      [ h1 [ txt "Reset Password" ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path "/root/request-reset-password")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") Message.Email ""
          ; submit_element
              language
              Pool_common.Message.(SendResetLink)
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  Page_layout.create_root_layout html message language
;;

let reset_password csrf message token Pool_context.{ language; _ } =
  let input_element = input_element language in
  let html =
    div
      [ h1 [ txt "Reset Password" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/reset-password")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Hidden (Some "token") Message.Token token
          ; input_element `Password (Some "password") Message.Password ""
          ; input_element
              `Password
              (Some "password_confirmation")
              Message.PasswordConfirmation
              ""
          ; submit_element
              language
              Pool_common.Message.(Save (Some password))
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  Page_layout.create_root_layout html message language
;;
