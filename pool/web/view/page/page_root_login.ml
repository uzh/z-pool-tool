open Tyxml.Html
open Component
module Message = Pool_common.Message

let login Pool_context.{ language; csrf; message; _ } =
  let input_element = input_element language in
  let html =
    div
      ~a:[ a_class [ "trim"; "narrow" ] ]
      [ div
          ~a:[ a_class [ "stack" ] ]
          [ h1
              ~a:[ a_class [ "heading-1" ] ]
              [ txt Pool_common.(Utils.text_to_string language I18n.LoginTitle)
              ]
          ; form
              ~a:
                [ a_action (Sihl.Web.externalize_path "/root/login")
                ; a_method `Post
                ; a_class [ "stack" ]
                ]
              [ csrf_element csrf ()
              ; input_element `Text Message.Field.Email
              ; input_element `Password Message.Field.Password
              ; submit_element language Message.Login ()
              ]
          ; p
              [ a
                  ~a:
                    [ a_href
                        (Sihl.Web.externalize_path
                           "/root/request-reset-password")
                    ]
                  [ txt
                      Pool_common.(
                        Utils.text_to_string language I18n.ResetPasswordLink)
                  ]
              ]
          ]
      ]
  in
  Page_layout.create_root_layout
    html
    message
    language
    ~active_navigation:"/root/login"
    ()
;;

let request_reset_password Pool_context.{ language; csrf; message; _ } =
  let input_element = input_element language in
  let html =
    div
      ~a:[ a_class [ "trim"; "narrow" ] ]
      [ h1 [ txt "Reset Password" ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path "/root/request-reset-password")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          [ csrf_element csrf ()
          ; input_element `Text Message.Field.Email
          ; submit_element language Message.SendResetLink ()
          ]
      ]
  in
  Page_layout.create_root_layout
    html
    message
    language
    ~active_navigation:"/root/request-reset-password"
    ()
;;

let reset_password token Pool_context.{ language; csrf; message; _ } =
  let html =
    div
      ~a:[ a_class [ "trim"; "narrow" ] ]
      [ h1 [ txt "Reset Password" ]
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/reset-password")
            ; a_method `Post
            ]
          [ csrf_element csrf ()
          ; input_element language `Hidden ~value:token Message.Field.Token
          ; input_element language `Password Message.Field.Password
          ; input_element language `Password Message.Field.PasswordConfirmation
          ; submit_element language Message.(Save (Some Field.password)) ()
          ]
      ]
  in
  Page_layout.create_root_layout
    html
    message
    language
    ~active_navigation:"/root/reset-password"
    ()
;;
