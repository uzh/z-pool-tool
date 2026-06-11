open Tyxml.Html
open Component.Input
open Pool_common.I18n

let txt_to_string lang m = txt (Pool_common.Utils.text_to_string lang m)

let import_confirmation_action =
  CCFun.flip Http_utils.externalize_path_with_params "/import-confirmation"
;;

let import_confirmation_form
      ?(terms_and_conditions : I18n.t option)
      ~action
      ~token
      ~submit
      Pool_context.{ language; csrf; _ }
  =
  let terms =
    CCOption.map_or
      ~default:[]
      (fun terms_and_conditions ->
         [ Component.Partials.terms_and_conditions_checkbox language terms_and_conditions
         ])
      terms_and_conditions
  in
  [ csrf_element csrf ()
  ; input_element
      language
      `Hidden
      Pool_message.Field.Token
      ~value:(User_import.Token.value token)
  ]
  @ terms
  @ [ div
        ~a:[ a_class [ "flexrow" ] ]
        [ button
            ~a:[ a_button_type `Submit; a_class [ "push"; "btn"; "primary" ] ]
            [ txt_to_string language submit ]
        ]
    ]
  |> form ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
;;

let import_confirmation ?terms_and_conditions ~active_after_import ~email ~token context =
  let Pool_context.{ language; query_parameters; _ } = context in
  let action = import_confirmation_action query_parameters in
  let to_string = txt_to_string language in
  let title, note, submit =
    if User_import.ActiveAfterImport.value active_after_import
    then ImportConfirmationTitle, ImportConfirmationNote, ConfirmAccountImport email
    else ActivateAccountTitle, ActivateAccountNote, ActivateAccountButton email
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin"; "stack" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ to_string title ]
    ; p [ to_string note ]
    ; import_confirmation_form ?terms_and_conditions ~action ~token ~submit context
    ]
;;
