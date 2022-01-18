open Tyxml.Html
open Component
module Message = Pool_common.Message

let login csrf message () =
  let input_element = input_element Pool_common.Language.En in
  let html =
    div
      [ h1 [ txt "Root Login" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/login")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") Message.Email ""
          ; input_element `Password (Some "password") Message.Password ""
          ; submit_element Pool_common.Language.En Pool_common.Message.(Login)
          ]
      ; a
          ~a:
            [ a_href (Sihl.Web.externalize_path "/root/request-reset-password")
            ]
          [ txt "Reset password" ]
      ]
  in
  Page_layout.create html message ()
;;

let request_reset_password csrf message () =
  let input_element = input_element Pool_common.Language.En in
  let html =
    div
      [ h1 [ txt "Reset Password" ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path "/root/request-reset-password")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") Message.Email ""
          ; submit_element
              Pool_common.Language.En
              Pool_common.Message.(SendResetLink)
          ]
      ]
  in
  Page_layout.create html message ()
;;

let reset_password csrf message token () =
  let input_element = input_element Pool_common.Language.En in
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
              Pool_common.Language.En
              Pool_common.Message.(Save (Some password))
          ]
      ]
  in
  Page_layout.create html message ()
;;
