open CCFun
open Containers
open Tyxml.Html
open Pool_common

let list { Pool_context.language; _ } filter_list query =
  let url = Uri.of_string "/admin/filter" in
  let sort =
    Component.DataTable.
      { url; query; language; search = Some Filter.searchable_by }
  in
  let cols =
    let create_filter : [ | Html_types.flow5 ] elt =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Component.Icon.Add
        ~classnames:[ "small" ]
        ~control:(language, Message.(Add (Some Field.Filter)))
        "/admin/filter/new"
    in
    [ `column Filter.column_title; `custom create_filter ]
  in
  let row (filter : Filter.t) =
    let open Filter in
    let title = Option.map Title.value filter.title in
    [ txt (Option.get_or ~default:"" title)
    ; Format.asprintf "/admin/filter/%s/edit" (Filter.Id.value filter.id)
      |> Component.Input.edit_link
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make ~target_id:"filters-list" ~cols ~row sort filter_list
;;

let index ({ Pool_context.language; _ } as context) filter_list query =
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Pool_common.(Utils.field_to_string language Message.Field.Filter)
             |> CCString.capitalize_ascii)
        ]
    ; list context filter_list query
    ]
;;

let edit
  { Pool_context.language; csrf; _ }
  filter
  key_list
  query_experiments
  query_tags
  =
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
          query_experiments
          query_tags)
    ]
;;
