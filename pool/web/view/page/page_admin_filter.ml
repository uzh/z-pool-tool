open Tyxml.Html

let index { Pool_context.language; _ } filter_list =
  let thead =
    Pool_common.Message.
      [ Field.Title |> Component.Table.field_to_txt language
      ; Component.Input.link_as_button
          ~style:`Success
          ~icon:`Add
          ~classnames:[ "small" ]
          ~control:(language, Pool_common.Message.(Add (Some Field.Filter)))
          "/admin/filter/new"
      ]
  in
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Pool_common.(Utils.field_to_string language Message.Field.Filter)
             |> CCString.capitalize_ascii)
        ]
    ; CCList.map
        (fun filter ->
          [ txt Filter.(filter.title |> CCOption.map_or ~default:"" Title.value)
          ; a
              ~a:
                [ a_href
                    (filter.Filter.id
                     |> Pool_common.Id.value
                     |> Format.asprintf "/admin/filter/%s/edit"
                     |> Sihl.Web.externalize_path)
                ]
              [ txt
                  Pool_common.(
                    Utils.control_to_string language (Message.edit None))
              ]
          ])
        filter_list
      |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
    ]
;;

let edit { Pool_context.language; csrf; _ } filter key_list query_experiments =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Component.Partials.form_title
        language
        Pool_common.Message.Field.Filter
        filter
    ; Component.Filter.(
        filter_form
          csrf
          language
          (Http_utils.Filter.Template filter)
          key_list
          []
          query_experiments)
    ]
;;
