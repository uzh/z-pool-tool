let terms
    csrf
    message
    user_id
    terms
    Pool_context.{ language; query_language; _ }
  =
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let submit_url =
    Format.asprintf "/terms-accepted/%s" user_id |> externalize
  in
  let children =
    let open Tyxml.Html in
    div
      [ h1
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.TermsAndConditionsTitle)
          ]
      ; p [ txt (terms |> Settings.TermsAndConditions.Terms.value) ]
      ; form
          ~a:[ a_action submit_url; a_method `Post ]
          [ Component.csrf_element csrf ()
          ; input
              ~a:[ a_input_type `Checkbox; a_name "_accepted"; a_required () ]
              ()
          ; a
              ~a:[ a_href ("/logout" |> externalize) ]
              [ txt
                  Pool_common.(Utils.control_to_string language Message.Decline)
              ]
          ; Component.submit_element
              language
              Pool_common.Message.(Accept (Some termsandconditions))
              ~classnames:[ "button--primary" ]
              ()
          ]
      ]
  in
  Page_layout.Tenant.create_layout children message language
;;
