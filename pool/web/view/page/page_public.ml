open Tyxml.Html
open Component

let create_layout = Page_layout.Tenant.create_layout
let txt_to_string lang m = [ txt (Pool_common.Utils.text_to_string lang m) ]

let index tenant message Pool_context.{ language; _ } =
  let text_to_string = Pool_common.Utils.text_to_string language in
  let html =
    div
      [ h1 [ txt (text_to_string Pool_common.I18n.HomeTitle) ]
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
  create_layout html message language
;;

let login csrf message Pool_context.{ language; query_language; _ } =
  let txt_to_string = txt_to_string language in
  let input_element = input_element language in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let open Pool_common in
  let html =
    div
      [ h1 (txt_to_string Pool_common.I18n.LoginTitle)
      ; form
          ~a:
            [ a_action (externalize "/login")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          [ csrf_element csrf ()
          ; input_element `Text (Some "email") Message.EmailAddress ""
          ; input_element `Password (Some "password") Message.Password ""
          ; submit_element
              language
              Message.Login
              ~classnames:[ "button--primary" ]
              ()
          ]
      ; a
          ~a:[ a_href (externalize "/request-reset-password") ]
          (txt_to_string Pool_common.I18n.ResetPasswordLink)
      ]
  in
  create_layout html message language
;;

let request_reset_password
    csrf
    message
    Pool_context.{ language; query_language; _ }
  =
  let input_element = input_element language in
  let html =
    div
      [ h1
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.ResetPasswordTitle)
          ]
      ; form
          ~a:
            [ a_action
                (HttpUtils.externalize_path_with_lang
                   query_language
                   "/request-reset-password")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          [ csrf_element csrf ()
          ; input_element
              `Text
              (Some "email")
              Pool_common.Message.EmailAddress
              ""
          ; submit_element
              language
              Pool_common.Message.(SendResetLink)
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  create_layout html message language
;;

let reset_password
    csrf
    message
    token
    Pool_context.{ language; query_language; _ }
  =
  let open Pool_common in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let input_element = input_element language in
  let html =
    div
      [ h1
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.ResetPasswordTitle)
          ]
      ; form
          ~a:[ a_action (externalize "/reset-password"); a_method `Post ]
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
              Message.(Save (Some password))
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  create_layout html message language
;;
