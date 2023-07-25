open Tyxml.Html

type calendar =
  | Location of Pool_location.Id.t
  | User

let calendar_endpoint calendar =
  Sihl.Web.externalize_path
  @@
  match calendar with
  | Location id ->
    Format.asprintf "/admin/sessions/location/%s" (Pool_location.Id.value id)
  | User -> "/admin/sessions"
;;

let hide_location = function
  | Location _ -> true
  | User -> false
;;

let data_attribs calendar =
  let action = [ a_user_data "calendar" (calendar_endpoint calendar) ] in
  if hide_location calendar
  then a_user_data "hide-location" "true" :: action
  else action
;;

let create calendar =
  div
    [ div ~a:[ a_id "calendar-notification" ] []
    ; div ~a:(data_attribs calendar) []
    ]
;;
