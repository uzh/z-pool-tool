open Tyxml.Html
open Pool_message
open Pool_common

let make_root language csrf announcement =
  let open Announcement in
  let id = "annoncement-id-banner" in
  let text = Text.find language announcement.text in
  let close =
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
          [ Component_icon.(to_html ~title:control Close) ]
      ]
  in
  [ Unsafe.data text ]
  |> Component_notification.create ~attributes:[ a_id id ] ~close language `Error
;;

let make_tenant language { Pool_context.Notitification.hint; style; link } =
  let html = p [ Utils.hint_to_string language hint |> txt ] in
  [ html ] |> Component_notification.create ?link language style
;;

let from_context langauge csrf =
  let open Pool_context.Notitification in
  let make_html = function
    | Root announcement -> make_root langauge csrf announcement
    | Tenant notification -> make_tenant langauge notification
  in
  function
  | [] -> None
  | announcements ->
    announcements
    |> CCList.map make_html
    |> div ~a:[ a_class [ "trim"; "safety-margin"; "gap"; "stack" ] ]
    |> CCOption.return
;;
