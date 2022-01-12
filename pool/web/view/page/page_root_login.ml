open Tyxml.Html
open Component

let login csrf message () =
  let html =
    div
      [ h1 [ txt "Root Login" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/login")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") ""
          ; input_element `Password (Some "password") ""
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
          ; input_element `Text (Some "email") ""
          ; submit_element
              Pool_common.Language.En
              Pool_common.Message.(SendResetLink)
          ]
      ]
  in
  Page_layout.create html message ()
;;

let reset_password csrf message token () =
  let html =
    div
      [ h1 [ txt "Reset Password" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/reset-password")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Hidden (Some "token") token
          ; input_element `Password (Some "password") ""
          ; input_element `Password (Some "password_confirmation") ""
          ; submit_element
              Pool_common.Language.En
              Pool_common.Message.(Save (Some password))
          ]
      ]
  in
  Page_layout.create html message ()
;;
