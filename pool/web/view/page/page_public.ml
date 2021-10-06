open Tyxml.Html

let index message () =
  let html = h1 [ txt "Welcome to Pool Tool" ] in
  Page_layout.create html message ()
;;

let login csrf message () =
  let html =
    div
      [ h1 [ txt "Login" ]
      ; form
          ~a:[ a_action (Sihl.Web.externalize_path "/login"); a_method `Post ]
          [ Component.csrf_element csrf ()
          ; input
              ~a:[ a_input_type `Text; a_name "email"; a_placeholder "email" ]
              ()
          ; input
              ~a:
                [ a_input_type `Password
                ; a_name "password"
                ; a_placeholder "password"
                ]
              ()
          ; input ~a:[ a_input_type `Submit; a_value "Login" ] ()
          ]
      ]
  in
  Page_layout.create html message ()
;;
