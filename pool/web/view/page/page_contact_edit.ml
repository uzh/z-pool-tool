open Tyxml.Html
open Component
open Input
module Message = Pool_common.Message
module HttpUtils = Http_utils

let contact_profile_layout language title html =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Pool_common.Utils.nav_link_to_string language title) ]
    ; html
    ]
;;

let grouped_custom_fields_form language custom_fields to_html =
  let open Custom_field in
  let groups, ungrouped_fields = custom_fields in
  div ~a:[ a_class [ "grid-col-2" ] ] (CCList.map to_html ungrouped_fields)
  :: CCList.map
       (fun (Group.Public.{ fields; _ } as group) ->
         div
           ~a:[ a_class [ "gap-lg" ] ]
           [ h2
               ~a:[ a_class [ "heading-2" ] ]
               [ txt Group.(Public.name language group) ]
           ; div ~a:[ a_class [ "grid-col-2" ] ] (fields |> CCList.map to_html)
           ])
       groups
;;

let personal_details_form
  csrf
  language
  query_language
  action
  tenant_languages
  contact
  custom_fields
  is_admin
  =
  let open Contact in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let action = externalize action in
  let form_attrs = [ a_method `Post; a_action action; a_class [ "stack" ] ] in
  let htmx_create field =
    Htmx.create ~required:true field language ~hx_post:action ()
  in
  let custom_field_to_html field =
    let hx_delete =
      Htmx.admin_profile_hx_delete
        (Contact.id contact)
        (Custom_field.Public.id field)
    in
    Htmx.custom_field_to_htmx
      language
      is_admin
      ~hx_post:action
      ~hx_delete
      field
      ()
  in
  let open Message in
  let static_fields =
    let fields =
      div
        ~a:[ a_class [ "grid-col-2" ] ]
        (csrf_element csrf ()
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
                      ; option_formatter = None
                      ; selected = contact.language
                      } )
                ; ( contact.paused_version
                  , Field.Paused
                  , contact.paused
                    |> User.Paused.value
                    |> CCOption.pure
                    |> boolean )
                ])
    in
    match is_admin with
    | true ->
      div
        ~a:[ a_class [ "inset"; "border"; "bg-grey-light" ] ]
        [ p
            [ txt
                Pool_common.(
                  Utils.hint_to_string
                    language
                    I18n.ContactProfileVisibleOverride)
            ]
        ; fields
        ]
    | false -> fields
  in
  form
    ~a:form_attrs
    [ static_fields
    ; div
        ~a:[ a_class [ "gap-lg" ] ]
        (grouped_custom_fields_form language custom_fields custom_field_to_html)
    ]
;;

let personal_details
  contact
  custom_fields
  tenant_languages
  Pool_context.{ language; query_language; csrf; _ }
  =
  let action = Htmx.contact_profile_hx_post in
  let is_admin = false in
  div
    [ div
        ~a:[ a_class [ "stack-lg" ] ]
        [ personal_details_form
            csrf
            language
            query_language
            action
            tenant_languages
            contact
            custom_fields
            is_admin
        ]
    ]
  |> contact_profile_layout language Pool_common.I18n.PersonalDetails
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
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          Pool_common.
            [ Utils.control_to_string
                language
                Message.(Update (Some Field.email))
              |> txt
            ]
      ; form
          ~a:(form_attrs "/user/update-email")
          [ csrf_element csrf ()
          ; input_element
              language
              `Email
              Message.Field.Email
              ~value:contact.user.Sihl_user.email
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.(Update (Some Field.Email))
                  ()
              ]
          ]
      ]
  in
  let password_form =
    let open Message in
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          Pool_common.
            [ Utils.control_to_string
                language
                Message.(Update (Some Field.password))
              |> txt
            ]
      ; form
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
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.(Update (Some Field.password))
                  ()
              ]
          ]
      ]
  in
  div
    [ div
        ~a:[ a_class [ "grid-col-2"; "gap-lg" ] ]
        [ email_form; password_form ]
    ]
  |> contact_profile_layout language Pool_common.I18n.LoginInformation
;;

let contact_information
  contact
  Pool_context.{ language; query_language; csrf; _ }
  (verification : Pool_user.UnverifiedPhoneNumber.t option)
  was_reset
  =
  let open Contact in
  let open Pool_common in
  let externalize = HttpUtils.externalize_path_with_lang query_language in
  let form_attrs action =
    [ a_method `Post; a_action (externalize action); a_class [ "stack" ] ]
  in
  let hint_to_html =
    CCFun.(Utils.hint_to_string language %> fun hint -> div [ txt hint ])
  in
  let form_title i18n =
    h2
      ~a:[ a_class [ "heading-3" ] ]
      Pool_common.[ Utils.control_to_string language i18n |> txt ]
  in
  let new_form () =
    let current_hint =
      (match contact.phone_number with
       | None -> I18n.ContactNoPhoneNumber
       | Some phone_number ->
         I18n.ContactCurrentPhoneNumber
           (Pool_user.PhoneNumber.value phone_number))
      |> hint_to_html
    in
    let reset_hint =
      match was_reset with
      | false -> txt ""
      | true ->
        [ I18n.ContactPhoneNumberVerificationWasReset |> hint_to_html ]
        |> Component.Notification.notification language `Success
    in
    div
      [ form_title Message.(Add (Some Field.PhoneNumber))
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ current_hint
          ; reset_hint
          ; form
              ~a:(form_attrs "/user/phone/update")
              [ csrf_element csrf ()
              ; input_element (* TODO: Add 2 part input (pre and nr) *)
                  language
                  `Text
                  Message.Field.PhoneNumber
              ; div
                  ~a:[ a_class [ "flexrow" ] ]
                  [ submit_element
                      ~classnames:[ "push" ]
                      language
                      Message.(Update (Some Field.PhoneNumber))
                      ()
                  ]
              ]
          ]
      ]
  in
  let form_as_link url i18n =
    form
      ~a:[ a_method `Post; a_action (Sihl.Web.externalize_path url) ]
      [ csrf_element csrf ()
      ; submit_element ~classnames:[ "as-link" ] language i18n ()
      ]
  in
  let verify_form phone_number =
    div
      [ form_title Message.(Verify (Some Field.PhoneNumber))
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ [ I18n.ContactEnterPhoneNumberToken
                (Pool_user.PhoneNumber.value phone_number)
              |> Utils.hint_to_string language
              |> txt
            ]
            |> Component.Notification.notification language `Warning
          ; form
              ~a:(form_attrs "/user/phone/verify")
              [ csrf_element csrf ()
              ; input_element (* TODO: Add 2 part input (pre and nr) *)
                  language
                  `Text
                  Message.Field.Token
              ; div
                  ~a:[ a_class [ "flexrow" ] ]
                  [ submit_element
                      ~classnames:[ "push" ]
                      language
                      Message.(Verify (Some Field.PhoneNumber))
                      ()
                  ]
              ]
          ; div
              ~a:[ a_class [ "flexrow"; "flex-gap"; "gap"; "justify-end" ] ]
              [ form_as_link
                  "/user/phone/resend-token"
                  Message.(Resend (Some Field.Token))
              ; form_as_link "/user/phone/reset" Message.EnterNewPhoneNumber
              ]
          ]
      ]
  in
  let form =
    match verification with
    | None -> new_form ()
    | Some { Pool_user.UnverifiedPhoneNumber.phone_number; _ } ->
      verify_form phone_number
  in
  div [ div ~a:[ a_class [ "grid-col-2"; "gap-lg" ] ] [ form ] ]
  |> contact_profile_layout language Pool_common.I18n.ContactInformation
;;
