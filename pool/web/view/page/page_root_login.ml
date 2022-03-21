open Tyxml.Html
open Component
module Message = Pool_common.Message

let login Pool_context.{ language; csrf; message; _ } =
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
          ; input_element `Text Message.Field.Email ""
          ; input_element `Password Message.Field.Password ""
          ; submit_element
              language
              Message.Login
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

let request_reset_password Pool_context.{ language; csrf; message; _ } =
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
          ; input_element `Text Message.Field.Email ""
          ; submit_element
              language
              Message.SendResetLink
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  Page_layout.create_root_layout html message language
;;

let reset_password token Pool_context.{ language; csrf; message; _ } =
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
          ; input_element `Hidden Message.Field.Token token
          ; input_element `Password Message.Field.Password ""
          ; input_element `Password Message.Field.PasswordConfirmation ""
          ; submit_element
              language
              Message.(Save (Some Field.password))
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  Page_layout.create_root_layout html message language
;;
