open Tyxml.Html

let terms csrf user_id terms Pool_context.{ language; query_language; _ } =
  let open Pool_common in
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let submit_url =
    Format.asprintf "/terms-accepted/%s" user_id |> externalize
  in
  div
    [ h1 [ txt (Utils.text_to_string language I18n.TermsAndConditionsTitle) ]
    ; p [ txt (terms |> Settings.TermsAndConditions.Terms.value) ]
    ; form
        ~a:[ a_action submit_url; a_method `Post ]
        [ Component.csrf_element csrf ()
        ; input
            ~a:
              [ a_input_type `Checkbox
              ; a_name Message.Field.(TermsAccepted |> show)
              ; a_required ()
              ]
            ()
        ; a
            ~a:[ a_href ("/logout" |> externalize) ]
            [ txt (Utils.control_to_string language Message.Decline) ]
        ; Component.submit_element
            language
            Message.(Accept (Some Field.termsandconditions))
            ~classnames:[ "button--primary" ]
            ()
        ]
    ]
;;
