open Tyxml.Html

(* TODO: Add EmailBouncing *)

type status_icon =
  | EmailVerified
  | Paused
  | Verified

let contact_has_status { Contact.paused; email_verified; verified; _ } =
  let open Pool_user in
  function
  | EmailVerified -> CCOption.is_none email_verified
  | Paused -> Paused.value paused
  | Verified -> CCOption.is_some verified
;;

let status_to_icon =
  let open Component_icon in
  function
  | EmailVerified -> CloseCircle (* TODO: Use correct icon *)
  | Paused -> NotificationsOffOutline
  | Verified -> CheckmarkCircleOutline
;;

let status_legend_text language =
  let open Pool_common in
  let field_to_string m =
    m |> Utils.field_to_string language |> CCString.capitalize_ascii
  in
  function
  | EmailVerified -> field_to_string Message.Field.EmailAddressVerified
  | Paused -> field_to_string Message.Field.Paused
  | Verified -> field_to_string Message.Field.Verified
;;

let email_status_icons = [ EmailVerified ]
let contact_status_icons = [ Paused; Verified ]
let all_status_icons = contact_status_icons @ email_status_icons

let icons_by_context = function
  | `All -> all_status_icons
  | `Name -> contact_status_icons
  | `Email -> email_status_icons
;;

let make_icons contact context =
  icons_by_context context
  |> CCList.filter (contact_has_status contact)
  |> CCList.map CCFun.(status_to_icon %> Component_icon.to_html)
;;

let status_icons_table_legend language context =
  let open Pool_common in
  let text_to_string m = m |> Utils.text_to_string language in
  let additional_items =
    if context = `All
    then
      [ ( text_to_string I18n.Disabled
        , Component_table.legend_color_item "bg-red-lighter" )
      ]
    else []
  in
  icons_by_context context
  |> CCList.map (fun status_icon ->
    ( status_legend_text language status_icon
    , status_to_icon status_icon
      |> Component_icon.to_html ~classnames:[ "legend-item" ] ))
  |> CCList.append additional_items
;;

let identity view_contact_name contact entity_id =
  if view_contact_name
  then Contact.lastname_firstname contact
  else Pool_common.Id.value entity_id
;;

let wrap_icons text icons =
  span [ txt text ] :: icons |> span ~a:[ a_class [ "flexrow"; "flex-gap-sm" ] ]
;;

let identity_with_icons ?(context = `Name) view_contact_name contact =
  let text =
    if view_contact_name
    then Contact.lastname_firstname contact
    else Pool_common.Id.value (Contact.id contact)
  in
  make_icons contact context |> wrap_icons text
;;

let email_with_icons contact =
  let text = Contact.email_address contact |> Pool_user.EmailAddress.value in
  make_icons contact `Email |> wrap_icons text
;;

let external_data_ids language external_data_ids =
  match external_data_ids with
  | [] ->
    p [ txt Pool_common.(Utils.text_to_string language I18n.EmptyListGeneric) ]
  | external_data_ids ->
    let thead =
      let open Pool_common in
      Message.Field.[ Experiment; Session; ExternalDataId ]
      |> CCList.map CCFun.(Utils.field_to_string_capitalized language %> txt)
    in
    external_data_ids
    |> CCList.map
         (fun
             Assignment.
               { ExternalDataIdentifier.external_data_id
               ; experiment_id
               ; experiment_title
               ; session_id
               ; session_start
               ; session_duration
               }
           ->
            let experiment_path =
              Format.asprintf
                "/admin/experiments/%s"
                Experiment.(Id.value experiment_id)
            in
            [ a
                ~a:[ a_href (experiment_path |> Sihl.Web.externalize_path) ]
                [ txt (Experiment.Title.value experiment_title) ]
            ; a
                ~a:
                  [ a_href
                      (Format.asprintf
                         "%s/sessions/%s"
                         experiment_path
                         (Session.Id.value session_id)
                       |> Sihl.Web.externalize_path)
                  ]
                [ txt
                    Session.(
                      Utils.Ptime.format_datetime_with_span
                        (Start.value session_start)
                        (Duration.value session_duration))
                ]
            ; txt Assignment.(ExternalDataId.value external_data_id)
            ])
    |> Component_table.horizontal_table ~thead `Striped
;;
