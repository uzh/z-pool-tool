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
