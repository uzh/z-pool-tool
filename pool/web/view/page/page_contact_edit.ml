open Tyxml.Html
open Component
module Message = Pool_common.Message

let detail contact Pool_context.{ language; query_language; _ } =
  let open Contact in
  let open Pool_common.I18n in
  let text_to_string = Pool_common.Utils.text_to_string language in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin"; "stack" ] ]
    [ div
        ([ h1 [ txt (text_to_string UserProfileTitle) ]
         ; p [ contact |> fullname |> Format.asprintf "Name: %s" |> txt ]
         ]
        @
        if contact.paused |> Pool_user.Paused.value
        then [ p [ txt (text_to_string UserProfilePausedNote) ] ]
        else [])
    ; p
        [ a
            ~a:
              [ a_href
                  (HttpUtils.externalize_path_with_lang
                     query_language
                     "/user/edit")
              ]
            [ txt
                Pool_common.(
                  Utils.control_to_string language (Message.Edit None))
            ]
        ]
    ]
;;

let edit
    user_update_csrf
    (contact : Contact.t)
    tenant_languages
    Pool_context.{ language; query_language; csrf; _ }
  =
  let open Contact in
  let open Pool_common.I18n in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let text_to_string = Pool_common.Utils.text_to_string language in
  let input_element = input_element language in
  let form_attrs action =
    [ a_method `Post; a_action (externalize action); a_class [ "stack" ] ]
  in
  let details_form =
    let action = "/user/update" in
    form
      ~a:(form_attrs action)
      (CCList.flatten
         [ [ Component.csrf_element csrf ~id:user_update_csrf () ]
         ; CCList.map
             (fun htmx_element ->
               Htmx.create htmx_element language ~hx_post:action ())
             Htmx.
               [ Firstname (contact.firstname_version, contact |> firstname)
               ; Lastname (contact.lastname_version, contact |> lastname)
               ; Paused (contact.paused_version, contact.paused)
               ; Language
                   (contact.language_version, contact.language, tenant_languages)
               ]
         ])
  in
  let email_form =
    form
      ~a:(form_attrs "/user/update-email")
      [ csrf_element csrf ()
      ; input_element `Email Message.Field.Email contact.user.Sihl_user.email
      ; submit_element language Message.(Update (Some Field.Email)) ()
      ]
  in
  let password_form =
    form
      ~a:(form_attrs "/user/update-password")
      ([ csrf_element csrf () ]
      @ CCList.map
          (fun m -> input_element `Password m "")
          [ Message.Field.CurrentPassword
          ; Message.Field.NewPassword
          ; Message.Field.PasswordConfirmation
          ]
      @ [ submit_element language Message.(Update (Some Field.password)) () ])
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (text_to_string UserProfileTitle) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ div
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ txt (text_to_string UserProfileDetailsSubtitle) ]
            ; details_form
            ]
        ; div
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ txt (text_to_string UserProfileLoginSubtitle) ]
            ; email_form
            ; password_form
            ]
        ; p
            [ a
                ~a:[ a_href (Sihl.Web.externalize_path "/user") ]
                [ txt
                    Pool_common.(Utils.control_to_string language Message.Back)
                ]
            ]
        ]
    ]
;;
