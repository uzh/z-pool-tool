let terms csrf message user_id terms =
  let submit_url =
    Format.asprintf "/participant/%s/terms-accepted" user_id
    |> Sihl.Web.externalize_path
  in
  let children =
    let open Tyxml.Html in
    div
      [ h1 [ txt "Terms and Conditions" ]
      ; p [ txt terms ]
      ; form
          ~a:[ a_action submit_url; a_method `Post ]
          [ input ~a:[ a_name "_csrf"; a_input_type `Hidden; a_value csrf ] ()
          ; div []
          ; input
              ~a:[ a_input_type `Checkbox; a_name "_accepted"; a_required () ]
              ()
          ; a
              ~a:[ a_href ("/logout" |> Sihl.Web.externalize_path) ]
              [ txt "Decline" ]
          ; button ~a:[ a_button_type `Submit ] [ txt "Accept" ]
          ]
      ]
  in
  Page_layout.create children message
;;
