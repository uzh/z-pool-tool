open Tyxml.Html
open Component

let txt_to_string lang m = [ txt (Pool_common.Utils.text_to_string lang m) ]

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
             (tenant.Pool_tenant.logos |> Pool_tenant.Logos.value))
      ]
  in
  Page_layout.create html message ()
;;

let login csrf language message () =
  let txt_to_string = txt_to_string language in
  let html =
    div
      [ h1 (txt_to_string Pool_common.I18n.LoginTitle)
      ; form
          ~a:[ a_action (Sihl.Web.externalize_path "/login"); a_method `Post ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") ""
          ; input_element `Password (Some "password") ""
          ; submit_element language Pool_common.Message.(Login)
          ]
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/request-reset-password") ]
          (txt_to_string Pool_common.I18n.ResetPasswordLink)
      ]
  in
  Page_layout.create html message ()
;;

let request_reset_password csrf language message () =
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
          ; submit_element language Pool_common.Message.(SendResetLink)
          ]
      ]
  in
  Page_layout.create html message ()
;;

let reset_password csrf language message token () =
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
          ; submit_element language Pool_common.Message.(Save (Some password))
          ]
      ]
  in
  Page_layout.create html message ()
;;
