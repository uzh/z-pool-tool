open Tyxml.Html
open Component.Input
module HttpUtils = Http_utils

let query_params_with_intended query_parameters intended =
  intended
  |> CCOption.map_or ~default:query_parameters (fun intended ->
    [ Pool_message.Field.Location, intended ] @ query_parameters)
;;

let login_token_confirmation
      Pool_context.{ language; query_parameters; csrf; _ }
      ?authentication_id
      ~email
      ?intended
      url
  =
  let query_parameters = query_params_with_intended query_parameters intended in
  let action = HttpUtils.externalize_path_with_params query_parameters url in
  let hint =
    Pool_common.I18n.LoginTokenSent (Pool_user.EmailAddress.value email)
    |> Pool_common.Utils.hint_to_string language
  in
  let hidden_input =
    let open Authentication in
    match authentication_id with
    | Some id -> input_element language `Hidden Pool_message.Field.Id ~value:(Id.value id)
    | None -> txt ""
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ Pool_common.(Utils.text_to_string language I18n.LoginTitle |> txt) ]
    ; Component.Notification.create language `Success [ txt hint ]
    ; div
        ~a:[ a_class [ "stack"; "gap" ] ]
        [ form
            ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
            [ csrf_element csrf ()
            ; hidden_input
            ; input_element language `Text Pool_message.Field.Token
            ; div
                ~a:[ a_class [ "flexrow"; "align-center"; "flex-gap" ] ]
                [ submit_element
                    ~classnames:[ "push" ]
                    language
                    Pool_message.Control.Login
                    ()
                ]
            ]
        ]
    ]
;;
