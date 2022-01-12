open Tyxml.Html
open Component
module Message = Pool_common.Message

let detail participant message () =
  let open Participant in
  let content =
    div
      [ div
          ([ h1 [ txt "User Profile" ]
           ; p [ participant |> fullname |> Format.asprintf "Name: %s" |> txt ]
           ]
          @
          if participant.paused |> Pool_user.Paused.value
          then
            [ p
                [ txt
                    "You paused all notifications for your user! (Click 'edit' \
                     to update this setting)"
                ]
            ]
          else [])
      ; a ~a:[ a_href (Sihl.Web.externalize_path "/user/edit") ] [ txt "Edit" ]
      ]
  in
  let html = div [ content ] in
  Page_layout.create html message ()
;;

let edit csrf language user_update_csrf participant message () =
  let open Participant in
  let id = participant |> id |> Pool_common.Id.value in
  let action = Sihl.Web.externalize_path "/user/update" in
  let details_form =
    form
      ~a:
        [ a_action action
        ; a_method `Post
        ; a_class [ "flex-wrap" ]
        ; a_user_data "id" id
        ]
      (CCList.flatten
         [ [ Component.csrf_element csrf ~id:user_update_csrf () ]
         ; CCList.map
             (fun (name, value, label, _type) ->
               hx_input_element
                 _type
                 name
                 value
                 (Participant.version_selector participant name
                 |> CCOption.get_exn_or
                      (Format.asprintf "No version found for field '%s'" name))
                 label
                 language
                 ~hx_post:action
                 ~hx_params:[ name ]
                 ())
             [ ( "firstname"
               , participant |> firstname |> Pool_user.Firstname.value
               , Message.firstname
               , `Text )
             ; ( "lastname"
               , participant |> lastname |> Pool_user.Lastname.value
               , Message.lastname
               , `Text )
             ; ( "paused"
               , participant.paused |> Pool_user.Paused.value |> string_of_bool
               , Message.paused
               , `Checkbox )
             ]
         ])
  in
  let email_form =
    form
      ~a:
        [ a_action (Sihl.Web.externalize_path "/user/update-email")
        ; a_method `Post
        ]
      [ csrf_element csrf ()
      ; input_element `Email (Some "email") participant.user.Sihl_user.email
      ; submit_element language Message.(Update (Some Message.email))
      ]
  in
  let password_form =
    form
      ~a:
        [ a_action (Sihl.Web.externalize_path "/user/update-password")
        ; a_method `Post
        ]
      [ csrf_element csrf ()
      ; input_element `Password (Some "current_password") ""
      ; input_element `Password (Some "new_password") ""
      ; input_element `Password (Some "password_confirmation") ""
      ; submit_element language Message.(Update (Some Message.password))
      ]
  in
  let html =
    div
      [ h1 [ txt "User Profile" ]
      ; div
          [ details_form
          ; hr ()
          ; h2 [ txt "Login Information" ]
          ; email_form
          ; password_form
          ]
      ; a ~a:[ a_href (Sihl.Web.externalize_path "/user") ] [ txt "Back" ]
      ]
  in
  Page_layout.create html message ()
;;
