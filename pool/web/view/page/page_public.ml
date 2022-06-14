open Tyxml.Html
open Component

let txt_to_string lang m = [ txt (Pool_common.Utils.text_to_string lang m) ]

let index tenant Pool_context.{ language; _ } welcome_text =
  let text_to_string = Pool_common.Utils.text_to_string language in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (text_to_string Pool_common.I18n.HomeTitle) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ div
            (CCList.map
               (fun logo ->
                 img
                   ~src:(Pool_common.File.path logo)
                   ~alt:""
                   ~a:[ a_style "width: 200px" ]
                   ())
               (tenant.Pool_tenant.logos |> Pool_tenant.Logos.value))
        ; div [ txt (I18n.content_to_string welcome_text) ]
        ]
    ]
;;

let login Pool_context.{ language; query_language; csrf; _ } =
  let txt_to_string = txt_to_string language in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        (txt_to_string Pool_common.I18n.LoginTitle)
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ form
            ~a:
              [ a_action (externalize "/login")
              ; a_method `Post
              ; a_class [ "stack" ]
              ]
            [ csrf_element csrf ()
            ; input_element language `Text Message.Field.Email
            ; input_element language `Password Message.Field.Password
            ; submit_element language Message.Login ()
            ]
        ; p
            [ a
                ~a:[ a_href (externalize "/request-reset-password") ]
                (txt_to_string Pool_common.I18n.ResetPasswordLink)
            ]
        ]
    ]
;;

let request_reset_password Pool_context.{ language; query_language; csrf; _ } =
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.text_to_string language I18n.ResetPasswordTitle)
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
        ; input_element language `Text Pool_common.Message.Field.Email
        ; submit_element language Pool_common.Message.SendResetLink ()
        ]
    ]
;;

let reset_password token Pool_context.{ language; query_language; csrf; _ } =
  let open Pool_common in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.ResetPasswordTitle) ]
    ; form
        ~a:
          [ a_action (externalize "/reset-password")
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ csrf_element csrf ()
        ; input_element language `Hidden Message.Field.Token ~value:token
        ; input_element language `Password Message.Field.Password
        ; input_element language `Password Message.Field.PasswordConfirmation
        ; submit_element language Message.(Save (Some Field.password)) ()
        ]
    ]
;;
