open Tyxml.Html
open Component.Input
open Pool_common.I18n
module HttpUtils = Http_utils

let txt_to_string lang m = txt (Pool_common.Utils.text_to_string lang m)

let import_confirmation_action =
  CCFun.flip HttpUtils.externalize_path_with_params "/import-confirmation"
;;

let password_fields language password_policy =
  div
    [ input_element
        ~hints:[ I18nText (password_policy |> I18n.content_to_string) ]
        ~required:true
        ~value:""
        language
        `Password
        Pool_message.Field.Password
    ; input_element
        ~required:true
        ~value:""
        language
        `Password
        Pool_message.Field.PasswordConfirmation
    ]
;;

let import_confirmation_form
      Pool_context.{ language; csrf; _ }
      ~action
      ~token
      ~terms_and_conditions
      ~submit
      ?password_fields
      ()
  =
  let password_fields = CCOption.value ~default:(txt "") password_fields in
  form
    ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
    [ csrf_element csrf ()
    ; input_element
        language
        `Hidden
        Pool_message.Field.Token
        ~value:(User_import.Token.value token)
    ; Component.Partials.terms_and_conditions_checkbox language terms_and_conditions
    ; password_fields
    ; div ~a:[ a_class [ "flexrow" ] ] [ submit ]
    ]
;;

let import_confirmation_already_active context token password_policy terms_and_conditions =
  let Pool_context.{ language; query_parameters; _ } = context in
  let to_string = txt_to_string language in
  let submit =
    submit_element
      ~classnames:[ "push" ]
      language
      Pool_message.Control.(Save (Some Field.password))
      ()
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin"; "stack" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ to_string ImportConfirmationTitle ]
    ; p [ to_string ImportConfirmationNote ]
    ; import_confirmation_form
        context
        ~action:(import_confirmation_action query_parameters)
        ~token
        ~terms_and_conditions
        ~submit
        ~password_fields:(password_fields language password_policy)
        ()
    ]
;;

let import_confirmation_needs_activation ~email context token terms_and_conditions =
  let Pool_context.{ language; query_parameters; _ } = context in
  let action = import_confirmation_action query_parameters in
  let to_string = txt_to_string language in
  let submit =
    button
      ~a:[ a_button_type `Submit; a_class [ "push"; "btn"; "primary" ] ]
      [ to_string (ActivateAccountButton email) ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin"; "stack" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ to_string ActivateAccountTitle ]
    ; p [ to_string ActivateAccountNote ]
    ; import_confirmation_form context ~action ~token ~terms_and_conditions ~submit ()
    ]
;;

let import_confirmation
      ~active_after_import
      ~email
      context
      token
      password_policy
      terms_and_conditions
  =
  if active_after_import
  then
    import_confirmation_already_active context token password_policy terms_and_conditions
  else import_confirmation_needs_activation ~email context token terms_and_conditions
;;
