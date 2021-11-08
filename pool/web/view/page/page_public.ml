open Tyxml.Html

let csrf_element = Component.csrf_element
let input_element = Component.input_element

let index message () =
  let html = h1 [ txt "Welcome to Pool Tool" ] in
  Page_layout.create html message ()
;;

let login csrf message () =
  let html =
    div
      [ h1 [ txt "Login" ]
      ; form
          ~a:[ a_action (Sihl.Web.externalize_path "/login"); a_method `Post ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") ""
          ; input_element `Password (Some "password") ""
          ; input_element `Submit None "Login"
          ]
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/request-reset-password") ]
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
            [ a_action (Sihl.Web.externalize_path "/request-reset-password")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") ""
          ; input_element `Submit None "Send reset link"
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
            [ a_action (Sihl.Web.externalize_path "/reset-password")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element `Hidden (Some "token") token
          ; input_element `Password (Some "password") ""
          ; input_element `Password (Some "password_confirmation") ""
          ; input_element `Submit None "Set new password"
          ]
      ]
  in
  Page_layout.create html message ()
;;
