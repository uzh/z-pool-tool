open Tyxml.Html

let status_icons { Contact.paused; verified; _ } =
  (* TODO: Add SMTP Bounce *)
  let open Pool_user in
  let open Component_icon in
  [ paused |> Paused.value, NotificationsOffOutline
  ; CCOption.is_some verified, CheckmarkCircleOutline
  ]
  |> CCList.filter_map (fun (show, icon) ->
    if show then Some (to_html icon) else None)
;;

let status_icons_table_legend language =
  let open Pool_common in
  let open Component_table in
  let open Component_icon in
  let field_to_string m =
    m |> Utils.field_to_string language |> CCString.capitalize_ascii
  in
  let text_to_string m = m |> Utils.text_to_string language in
  [ text_to_string I18n.Disabled, legend_color_item "bg-red-lighter"
  ; field_to_string Message.Field.Paused, legend_icon_item NotificationsOutline
  ; ( field_to_string Message.Field.Verified
    , legend_icon_item CheckmarkCircleOutline )
  ]
;;

let identity view_contact_name contact entity_id =
  if view_contact_name
  then Contact.lastname_firstname contact
  else Pool_common.Id.value entity_id
;;

let identity_with_icons view_contact_name contact entity_id =
  let text =
    if view_contact_name
    then Contact.lastname_firstname contact
    else Pool_common.Id.value entity_id
  in
  span [ txt text ] :: status_icons contact
  |> div ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
;;
