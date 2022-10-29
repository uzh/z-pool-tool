open Tyxml.Html
open Component.Input

let terms user_id terms Pool_context.{ language; query_language; csrf; _ } =
  let open Pool_common in
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let submit_url =
    Format.asprintf "/terms-accepted/%s" user_id |> externalize
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.TermsAndConditionsTitle) ]
    ; p
        [ terms
          |> Settings.TermsAndConditions.Terms.value
          |> Http_utils.add_line_breaks
        ]
    ; form
        ~a:[ a_action submit_url; a_method `Post; a_class [ "stack" ] ]
        [ csrf_element csrf ()
        ; checkbox_element
            language
            Pool_common.Message.Field.TermsAccepted
            ~required:true
        ; div
            ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
            [ submit_element
                language
                Message.(Accept (Some Field.termsandconditions))
                ()
            ; a
                ~a:[ a_href ("/logout" |> externalize) ]
                [ txt (Utils.control_to_string language Message.Decline) ]
            ]
        ]
    ]
;;
