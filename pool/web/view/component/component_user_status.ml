open CCFun.Infix
open Tyxml.Html
(* TODO: Add EmailBouncing *)

type status_icon =
  | EmailUnverified
  | Inactive
  | Paused
  | Verified

let status_to_icon =
  let open Component_icon in
  function
  | EmailUnverified -> MailError
  | Inactive -> TrashOutline
  | Paused -> NotificationsOffOutline
  | Verified -> CheckmarkCircleOutline
;;

let status_legend_text language =
  let open Pool_common in
  let field_to_string =
    Utils.field_to_string language %> CCString.capitalize_ascii
  in
  let open Pool_message.Field in
  function
  | EmailUnverified -> field_to_string EmailAddressUnverified
  | Inactive -> field_to_string Inactive
  | Paused -> field_to_string Paused
  | Verified -> field_to_string Verified
;;

let make_table_legend ?(additional_items = []) language icons =
  icons
  |> CCList.map (fun status_icon ->
    ( status_legend_text language status_icon
    , status_to_icon status_icon
      |> Component_icon.to_html ~classnames:[ "legend-item" ] ))
  |> CCList.append additional_items
  |> Component_table.table_legend
;;

let wrap_icons text icons =
  span [ txt text ] :: icons |> span ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
;;

module Contact = struct
  let has_status { Contact.paused; email_verified; verified; user; _ } =
    let open Pool_user in
    function
    | EmailUnverified -> CCOption.is_none email_verified
    | Inactive -> Pool_user.(equal_status user.status Inactive)
    | Paused -> Paused.value paused
    | Verified -> CCOption.is_some verified
  ;;

  let email_status_icons = [ EmailUnverified ]
  let contact_status_icons = [ Inactive; Paused; Verified ]
  let all_status_icons = contact_status_icons @ email_status_icons

  let icons_by_context = function
    | `All -> all_status_icons
    | `Name -> contact_status_icons
    | `Email -> email_status_icons
  ;;

  let make_icons language contact context =
    icons_by_context context
    |> CCList.filter (has_status contact)
    |> CCList.map (fun icon ->
      icon
      |> status_to_icon
      |> Component_icon.to_html ~title:(status_legend_text language icon))
  ;;

  let status_icons_table_legend language context =
    let open Pool_common in
    let additional_items =
      if context = `All
      then
        [ ( Utils.text_to_string language I18n.Disabled
          , Component_table.legend_color_item "bg-red-lighter" )
        ]
      else []
    in
    icons_by_context context |> make_table_legend ~additional_items language
  ;;

  let identity view_contact_name contact entity_id =
    if view_contact_name
    then Contact.lastname_firstname contact
    else Pool_common.Id.value entity_id
  ;;

  let identity_with_icons ?(context = `Name) language view_contact_name contact =
    let text =
      if view_contact_name
      then Contact.lastname_firstname contact
      else Pool_common.Id.value (Contact.id contact)
    in
    make_icons language contact context |> wrap_icons text
  ;;

  let email_with_icons language contact =
    let text = Contact.email_address contact |> Pool_user.EmailAddress.value in
    make_icons language contact `Email |> wrap_icons text
  ;;
end

module Admin = struct
  let email_status_icons = [ EmailUnverified ]

  let has_status { Admin.email_verified; user; _ } = function
    | EmailUnverified -> CCOption.is_none email_verified
    | Inactive -> Pool_user.(equal_status Inactive user.status)
    | Paused | Verified -> false
  ;;

  let make_icons admin =
    email_status_icons
    |> CCList.filter (has_status admin)
    |> CCList.map CCFun.(status_to_icon %> Component_icon.to_html)
  ;;

  let status_icons_table_legend language =
    make_table_legend language email_status_icons
  ;;

  let email_with_icons admin =
    let text = Admin.email_address admin |> Pool_user.EmailAddress.value in
    make_icons admin |> wrap_icons text
  ;;
end
