open Tyxml.Html
open Component
module Message = Pool_common.Message

let detail participant message Pool_context.{ language; query_language; _ } =
  let open Participant in
  let text_to_string = Pool_common.Utils.text_to_string language in
  let content =
    div
      [ div
          ([ h1 [ txt (text_to_string Pool_common.I18n.UserProfileTitle) ]
           ; p [ participant |> fullname |> Format.asprintf "Name: %s" |> txt ]
           ]
          @
          if participant.paused |> Pool_user.Paused.value
          then
            [ p [ txt (text_to_string Pool_common.I18n.UserProfilePausedNote) ]
            ]
          else [])
      ; a
          ~a:
            [ a_href
                (HttpUtils.externalize_path_with_lang
                   query_language
                   "/user/edit")
            ]
          [ txt
              Pool_common.(Utils.control_to_string language (Message.Edit None))
          ]
      ]
  in
  let html = div [ content ] in
  Page_layout.create html message language
;;

let edit
    csrf
    user_update_csrf
    participant
    message
    Pool_context.{ language; query_language; _ }
  =
  let open Participant in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let action = externalize "/user/update" in
  let text_to_string = Pool_common.Utils.text_to_string language in
  let input_element = input_element language in
  let details_form =
    form
      ~a:[ a_action action; a_method `Post ]
      (CCList.flatten
         [ [ Component.csrf_element csrf ~id:user_update_csrf () ]
         ; CCList.map
             (fun (name, value, label, _type) ->
               hx_input_element
                 _type
                 name
                 value
                 (Participant.version_selector participant name
                 |> CCOption.get_exn_or
                      Pool_common.(
                        Utils.error_to_string
                          language
                          (Message.HtmxVersionNotFound name)))
                 label
                 language
                 ~hx_post:action
                 ~hx_params:[ name ]
                 ())
             [ ( "firstname"
               , participant |> firstname |> Pool_user.Firstname.value
               , Message.firstname
               , `Text )
             ; ( "lastname"
               , participant |> lastname |> Pool_user.Lastname.value
               , Message.lastname
               , `Text )
             ; ( "paused"
               , participant.paused |> Pool_user.Paused.value |> string_of_bool
               , Message.paused
               , `Checkbox )
             ]
         ])
  in
  let email_form =
    form
      ~a:[ a_action (externalize "/user/update-email"); a_method `Post ]
      [ csrf_element csrf ()
      ; input_element
          `Email
          (Some "email")
          Message.Email
          participant.user.Sihl_user.email
      ; submit_element
          language
          Message.(Update (Some Message.email))
          ~classnames:[ "button--primary" ]
          ()
      ]
  in
  let password_form =
    form
      ~a:[ a_action (externalize "/user/update-password"); a_method `Post ]
      [ csrf_element csrf ()
      ; input_element
          `Password
          (Some "current_password")
          Message.CurrentPassword
          ""
      ; input_element `Password (Some "new_password") Message.NewPassword ""
      ; input_element
          `Password
          (Some "password_confirmation")
          Message.PasswordConfirmation
          ""
      ; submit_element
          language
          Message.(Update (Some Message.password))
          ~classnames:[ "button--primary" ]
          ()
      ]
  in
  let html =
    div
      [ h1 [ txt (text_to_string Pool_common.I18n.UserProfileTitle) ]
      ; div
          [ details_form
          ; hr ()
          ; h2
              [ txt (text_to_string Pool_common.I18n.UserProfileLoginSubtitle) ]
          ; email_form
          ; password_form
          ]
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/user") ]
          [ txt Pool_common.(Utils.control_to_string language Message.Back) ]
      ]
  in
  Page_layout.create html message language
;;
