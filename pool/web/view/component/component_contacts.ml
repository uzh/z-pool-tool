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
                      Time.format_start_end_with_duration
                        (Start.value session_start)
                        (Duration.value session_duration))
                ]
            ; txt Assignment.(ExternalDataId.value external_data_id)
            ])
    |> Component_table.horizontal_table ~thead `Striped
;;
