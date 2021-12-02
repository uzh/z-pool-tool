open Tyxml.Html

let csrf_element = Component.csrf_element
let input_element = Component.input_element

let index tenant message () =
  let html =
    div
      [ h1 [ txt "Welcome to Pool Tool" ]
      ; div
          (CCList.map
             (fun logo ->
               img
                 ~src:(Pool_common.File.path logo)
                 ~alt:""
                 ~a:[ a_style "width: 200px" ]
                 ())
             (tenant.Tenant.logos |> Tenant.Logos.value))
      ]
  in
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

let error_page ?(lang = Pool_common.Language.En) message () =
  let html =
    div
      [ h1 [ txt "An error occurred" ]
      ; div [ txt (Pool_common.Utils.error_to_string lang message) ]
      ]
  in
  Page_layout.create html None ()
;;
