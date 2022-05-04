open Tyxml.Html
open Component

let txt_to_string lang m = [ txt (Pool_common.Utils.text_to_string lang m) ]

let index tenant Pool_context.{ language; _ } welcome_text =
  let text_to_string = Pool_common.Utils.text_to_string language in
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
    ; div [ txt (I18n.content_to_string welcome_text) ]
    ]
;;

let login Pool_context.{ language; query_language; csrf; _ } =
  let txt_to_string = txt_to_string language in
  let input_element = input_element language in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let open Pool_common in
  div
    [ h1 (txt_to_string Pool_common.I18n.LoginTitle)
    ; form
        ~a:
          [ a_action (externalize "/login")
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
        ~a:[ a_href (externalize "/request-reset-password") ]
        (txt_to_string Pool_common.I18n.ResetPasswordLink)
    ]
;;

let request_reset_password Pool_context.{ language; query_language; csrf; _ } =
  let input_element = input_element language in
  div
    [ h1
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
        ; input_element `Text Pool_common.Message.Field.Email ""
        ; submit_element
            language
            Pool_common.Message.SendResetLink
            ~classnames:[ "button--primary" ]
            ()
        ]
    ]
;;

let reset_password token Pool_context.{ language; query_language; csrf; _ } =
  let open Pool_common in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let input_element = input_element language in
  div
    [ h1 [ txt (Utils.text_to_string language I18n.ResetPasswordTitle) ]
    ; form
        ~a:[ a_action (externalize "/reset-password"); a_method `Post ]
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
;;
