open Tyxml.Html

let index { Pool_context.language; _ } filter_list =
  let thead = Pool_common.Message.Field.[ Some Title; None ] in
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Pool_common.(Utils.field_to_string language Message.Field.Filter)
            |> CCString.capitalize_ascii)
        ]
    ; p
        [ a
            ~a:[ a_href (Sihl.Web.externalize_path "/admin/filter/new") ]
            [ txt
                Pool_common.(
                  Utils.control_to_string
                    language
                    Message.(Create (Some Field.Filter)))
            ]
        ]
    ; CCList.map
        (fun filter ->
          [ txt Filter.(filter.title |> CCOption.map_or ~default:"" Title.value)
          ; a
              ~a:
                [ a_href
                    (Format.asprintf
                       "/admin/filter/%s/edit"
                       (Pool_common.Id.value filter.Filter.id))
                ]
              [ txt
                  Pool_common.(
                    Utils.control_to_string language (Message.edit None))
              ]
          ])
        filter_list
      |> Component.Table.horizontal_table `Striped language ~thead
    ]
;;

let edit { Pool_context.language; csrf; _ } filter key_list =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Component.Partials.form_title
        language
        Pool_common.Message.Field.Filter
        filter
    ; Component.Filter.(
        filter_form csrf language (FilterParam filter) key_list [])
    ]
;;
