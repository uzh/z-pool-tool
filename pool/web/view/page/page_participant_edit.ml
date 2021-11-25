open Tyxml.Html
open Component

let detail participant message () =
  let open Participant in
  let content =
    div
      [ div
          ([ h1 [ txt "User Profile" ]
           ; p [ participant |> fullname |> Format.asprintf "Name: %s" |> txt ]
           ]
          @
          if participant.paused |> Common_user.Paused.value
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

let edit csrf participant message () =
  let open Participant in
  let id = participant |> id |> Pool_common.Id.value in
  let action = Sihl.Web.externalize_path "/user/update" in
  let form =
    form
      ~a:
        [ a_action action
        ; a_method `Post
        ; a_class [ "flex-wrap" ]
        ; a_user_data "id" id
        ]
      (CCList.flatten
         [ [ Component.csrf_element csrf () ]
         ; CCList.map
             (fun (name, value, _type) ->
               hx_input_element
                 _type
                 name
                 value
                 ~hx_post:action
                 ~hx_params:[ name ]
                 ~hx_target:
                   (Format.asprintf
                      "form[data-id='%s'] div[data-name='%s']"
                      id
                      name)
                 ())
             [ ( "firstname"
               , participant |> firstname |> Common_user.Firstname.value
               , `Text )
             ; ( "lastname"
               , participant |> lastname |> Common_user.Lastname.value
               , `Text )
             ; ( "paused"
               , participant.paused
                 |> Common_user.Paused.value
                 |> Utils.Bool.stringify
               , `Checkbox )
             ]
         ])
  in
  let html =
    div
      [ h1 [ txt "User Profile" ]
      ; div [ form ]
      ; a ~a:[ a_href (Sihl.Web.externalize_path "/user") ] [ txt "Back" ]
      ]
  in
  Page_layout.create html message ()
;;
