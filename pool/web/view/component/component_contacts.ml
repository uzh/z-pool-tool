open Tyxml.Html

let external_data_ids language external_data_ids =
  match external_data_ids with
  | [] -> p [ txt Pool_common.(Utils.text_to_string language I18n.EmptyListGeneric) ]
  | external_data_ids ->
    let thead =
      let open Pool_common in
      Pool_message.Field.[ Experiment; Session; ExternalDataId ]
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
              Format.asprintf "/admin/experiments/%s" Experiment.(Id.value experiment_id)
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
                      Utils.Ptime.format_start_end_with_duration
                        (Start.value session_start)
                        (Duration.value session_duration))
                ]
            ; txt Assignment.(ExternalDataId.value external_data_id)
            ])
    |> Component_table.horizontal_table ~thead `Striped
;;

let contact_lastname_firstname access_contact_profiles contact =
  let text = contact |> Contact.lastname_firstname |> txt in
  match access_contact_profiles with
  | true ->
    a
      ~a:
        [ a_href
            (Http_utils.Url.Admin.contact_path ~id:(Contact.id contact) ()
             |> Sihl.Web.externalize_path)
        ]
      [ text ]
  | false -> text
;;

let column_name ~view_contact_name language : Data_table.col =
  match view_contact_name with
  | true -> `column Pool_user.column_name
  | false ->
    `custom
      (txt (Pool_common.Utils.field_to_string_capitalized language Pool_message.Field.Id))
;;

let cell_name ~view_contact_name ~access_contact_profiles contact
  : [< Html_types.td_content_fun ] elt * Pool_message.Field.t option
  =
  let open Pool_message in
  match view_contact_name with
  | true -> contact_lastname_firstname access_contact_profiles contact, Some Field.Name
  | false ->
    ( span ~a:[ a_class [ "nowrap" ] ] [ txt Contact.(contact |> id |> Id.value) ]
    , Some Field.Contact )
;;
