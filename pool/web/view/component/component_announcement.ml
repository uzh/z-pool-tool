open Tyxml.Html
open Pool_message
open Pool_common
open Announcement

let make language csrf announcement =
  let id = "annoncement-id-banner" in
  let text = Text.find language announcement.text in
  let hide_button =
    let url =
      Http_utils.Url.announcement_path ~id:announcement.id ~suffix:"hide" ()
      |> Sihl.Web.externalize_path
    in
    let control =
      Control.Hide (Some Field.Announcement) |> Utils.control_to_string language
    in
    form
      ~a:[ a_class [ "notification-close" ] ]
      [ Component_input.csrf_element csrf ()
      ; span
          ~a:
            [ a_user_data "hx-post" url
            ; a_user_data "hx-target" ("#" ^ id)
            ; a_user_data "hx-params" "_csrf"
            ; a_user_data "hx-trigger" "click"
            ; a_user_data "hx-swap" "outerHTML"
            ; a_class [ "pointer" ]
            ]
          [ Component_icon.(
              to_html ~classnames:[ "notification-close" ] ~title:control Close)
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "gap" ]; a_id id ]
    [ div
        ~a:[ a_class [ "notification"; "error"; "announcement" ] ]
        [ hide_button; div [ Unsafe.data text ] ]
    ]
;;
