open Tyxml.Html
open Component.Input
open Pool_message

let terms
  ?notification
  terms
  Pool_context.{ language; query_parameters; csrf; _ }
  =
  let terms = terms |> I18n.content_to_string |> Unsafe.data in
  let open Pool_common in
  let externalize =
    Http_utils.externalize_path_with_params
      (Pool_context.Utils.remove_query_param query_parameters Field.Redirected)
  in
  let notifiaction =
    match notification with
    | Some notification ->
      Component.Notification.notification
        language
        `Warning
        [ p [ txt (Utils.text_to_string language notification) ] ]
    | None -> txt ""
  in
  let submit_url = "/terms-accepted" |> externalize in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.TermsAndConditionsTitle) ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ notifiaction
        ; div [ terms ]
        ; form
            ~a:[ a_action submit_url; a_method `Post; a_class [ "stack" ] ]
            [ csrf_element csrf ()
            ; checkbox_element language Field.TermsAccepted ~required:true
            ; div
                ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                [ a
                    ~a:[ a_href ("/logout" |> externalize) ]
                    [ txt (Utils.control_to_string language Control.Decline) ]
                ; submit_element
                    ~classnames:[ "push" ]
                    language
                    (Control.Accept (Some Field.termsandconditions))
                    ()
                ]
            ]
        ]
    ]
;;
