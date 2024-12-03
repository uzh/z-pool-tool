open Tyxml.Html
open Component.Input
open Pool_message

let login ?intended Pool_context.{ language; csrf; _ } =
  let input_element = input_element language in
  let action =
    HttpUtils.intended_or "/root/login" intended |> Sihl.Web.externalize_path
  in
  div
    ~a:[ a_class [ "trim"; "narrow" ] ]
    [ div
        ~a:[ a_class [ "stack" ] ]
        [ h1
            ~a:[ a_class [ "heading-1" ] ]
            [ txt Pool_common.(Utils.text_to_string language I18n.LoginTitle) ]
        ; form
            ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
            [ csrf_element csrf ()
            ; input_element `Text Field.Email
            ; input_element `Password Field.Password
            ; div
                ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                [ div
                    [ a
                        ~a:
                          [ a_href
                              (Sihl.Web.externalize_path "/root/request-reset-password")
                          ]
                        [ txt
                            Pool_common.(
                              Utils.text_to_string language I18n.ResetPasswordLink)
                        ]
                    ]
                ; submit_element ~classnames:[ "push" ] language Control.Login ()
                ]
            ]
        ]
    ]
;;

let request_reset_password Pool_context.{ language; csrf; _ } =
  let input_element = input_element language in
  div
    ~a:[ a_class [ "trim"; "narrow" ] ]
    [ h1 [ txt "Reset Password" ]
    ; form
        ~a:
          [ a_action (Sihl.Web.externalize_path "/root/request-reset-password")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ csrf_element csrf ()
        ; input_element `Text Field.Email
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element ~classnames:[ "push" ] language Control.SendResetLink () ]
        ]
    ]
;;

let reset_password token Pool_context.{ language; csrf; _ } =
  div
    ~a:[ a_class [ "trim"; "narrow" ] ]
    [ h1 [ txt "Reset Password" ]
    ; form
        ~a:
          [ a_action (Sihl.Web.externalize_path "/root/reset-password")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ csrf_element csrf ()
        ; input_element language `Hidden ~value:token Field.Token
        ; input_element language `Password Field.Password
        ; input_element language `Password Field.PasswordConfirmation
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element
                ~classnames:[ "push" ]
                language
                (Control.Save (Some Field.password))
                ()
            ]
        ]
    ]
;;
