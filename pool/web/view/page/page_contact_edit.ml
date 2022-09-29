open Tyxml.Html
open Component
module Message = Pool_common.Message

let contact_profile_layout language title ?active html =
  let open Pool_common in
  let subnav_links =
    I18n.
      [ Overview, "/"
      ; PersonalDetails, "/personal-details"
      ; LoginInformation, "/login-information"
      ]
  in
  let base_url = "/user" in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ Component.Navigation.subnav language subnav_links base_url active
    ; h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.nav_link_to_string language title) ]
    ; html
    ]
;;

let personal_details_form
  csrf
  user_update_csrf
  language
  query_language
  action
  tenant_languages
  contact
  custom_fields
  =
  let open Contact in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let action = externalize action in
  let form_attrs = [ a_method `Post; a_action action; a_class [ "stack" ] ] in
  let htmx_create field = Htmx.create field language ~hx_post:action () in
  let custom_fields_form =
    custom_fields
    |> CCList.map (fun custom_field ->
         Htmx.custom_field_to_htmx language custom_field ~hx_post:action ())
  in
  let open Pool_common.Message in
  form
    ~a:form_attrs
    [ div
        ~a:[ a_class [ "stack" ] ]
        (Component.csrf_element csrf ~id:user_update_csrf ()
        :: CCList.map
             (fun (version, field, value) ->
               Htmx.create_entity version field value |> htmx_create)
             Htmx.
               [ ( contact.firstname_version
                 , Field.Firstname
                 , Text
                     (contact
                     |> Contact.firstname
                     |> User.Firstname.value
                     |> CCOption.pure) )
               ; ( contact.lastname_version
                 , Field.Lastname
                 , Text
                     (contact
                     |> Contact.lastname
                     |> User.Lastname.value
                     |> CCOption.pure) )
               ; ( contact.language_version
                 , Field.Language
                 , Select
                     { show = Pool_common.Language.show
                     ; options = tenant_languages
                     ; selected = contact.language
                     } )
               ; ( contact.paused_version
                 , Field.Paused
                 , Checkbox (contact.paused |> User.Paused.value) )
               ])
    ; div ~a:[ a_class [ "stack" ] ] custom_fields_form
    ]
;;

let detail contact Pool_context.{ language; query_language; _ } =
  let open Contact in
  let text_to_string = Pool_common.Utils.text_to_string language in
  div
    [ div
        ([ p [ contact |> fullname |> Format.asprintf "Name: %s" |> txt ] ]
        @
        if contact.paused |> Pool_user.Paused.value
        then
          [ p [ txt (text_to_string Pool_common.I18n.UserProfilePausedNote) ] ]
        else [])
    ; p
        [ a
            ~a:
              [ a_href
                  (HttpUtils.externalize_path_with_lang
                     query_language
                     "/user/personal-details")
              ]
            [ txt
                Pool_common.(
                  Utils.control_to_string language (Message.Edit None))
            ]
        ]
    ]
  |> contact_profile_layout
       language
       Pool_common.I18n.Overview
       ~active:Pool_common.I18n.Overview
;;

let personal_details
  user_update_csrf
  (contact : Contact.t)
  custom_fields
  tenant_languages
  Pool_context.{ language; query_language; csrf; _ }
  =
  let action = Htmx.contact_profile_hx_post in
  div
    [ div
        ~a:[ a_class [ "stack-lg" ] ]
        [ personal_details_form
            csrf
            user_update_csrf
            language
            query_language
            action
            tenant_languages
            contact
            custom_fields
        ; p
            [ a
                ~a:[ a_href (Sihl.Web.externalize_path "/user") ]
                [ txt
                    Pool_common.(Utils.control_to_string language Message.Back)
                ]
            ]
        ]
    ]
  |> contact_profile_layout
       language
       Pool_common.I18n.PersonalDetails
       ~active:Pool_common.I18n.PersonalDetails
;;

let login_information
  (contact : Contact.t)
  Pool_context.{ language; query_language; csrf; _ }
  password_policy
  =
  let open Contact in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let form_attrs action =
    [ a_method `Post; a_action (externalize action); a_class [ "stack" ] ]
  in
  let email_form =
    form
      ~a:(form_attrs "/user/update-email")
      [ csrf_element csrf ()
      ; input_element
          language
          `Email
          Message.Field.Email
          ~value:contact.user.Sihl_user.email
      ; submit_element language Message.(Update (Some Field.Email)) ()
      ]
  in
  let password_form =
    let open Message in
    form
      ~a:(form_attrs "/user/update-password")
      [ csrf_element csrf ()
      ; input_element
          language
          `Password
          ~value:""
          Field.CurrentPassword
          ~required:true
      ; input_element
          language
          ~help:
            Pool_common.I18n.(
              I18nText (password_policy |> I18n.content_to_string))
          `Password
          ~value:""
          Field.NewPassword
          ~required:true
      ; input_element
          language
          `Password
          ~value:""
          Field.PasswordConfirmation
          ~required:true
      ; submit_element language Message.(Update (Some Field.password)) ()
      ]
  in
  div
    [ div ~a:[ a_class [ "stack-lg" ] ] [ email_form; password_form ]
    ; p
        [ a
            ~a:[ a_href (Sihl.Web.externalize_path "/user") ]
            [ txt Pool_common.(Utils.control_to_string language Message.Back) ]
        ]
    ]
  |> contact_profile_layout
       language
       Pool_common.I18n.LoginInformation
       ~active:Pool_common.I18n.LoginInformation
;;
