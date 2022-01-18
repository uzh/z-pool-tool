let terms csrf language message user_id terms =
  let submit_url =
    Format.asprintf "/terms-accepted/%s" user_id |> Sihl.Web.externalize_path
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
              ~a:[ a_href ("/logout" |> Sihl.Web.externalize_path) ]
              [ txt
                  Pool_common.(Utils.control_to_string language Message.Decline)
              ]
          ; Component.submit_element
              language
              Pool_common.Message.(Accept (Some termsandconditions))
          ]
      ]
  in
  Page_layout.create children message
;;
