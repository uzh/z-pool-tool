open CCFun
open Containers
open Tyxml.Html

let list { Pool_context.language; _ } filter_list query =
  let url = Uri.of_string "/admin/filter" in
  let data_table =
    Component.DataTable.create_meta ~search:Filter.searchable_by url query language
  in
  let cols =
    let create_filter : [ | Html_types.flow5 ] elt =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Component.Icon.Add
        ~classnames:[ "small" ]
        ~control:(language, Pool_message.(Control.Add (Some Field.Filter)))
        "/admin/filter/new"
    in
    [ `column Filter.column_title; `custom create_filter ]
  in
  let row (filter : Filter.t) =
    let open Filter in
    let title = CCOption.map Title.value filter.title in
    [ txt (Option.get_or ~default:"" title)
    ; Format.asprintf "/admin/filter/%s/edit" (Filter.Id.value filter.id)
      |> Component.Input.edit_link
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"filters-list"
    ~th_class:[ "w-10"; "w-2" ]
    ~cols
    ~row
    data_table
    filter_list
;;

let index ({ Pool_context.language; _ } as context) filter_list query =
  let hint =
    [ Pool_common.(Utils.hint_to_string language I18n.FilterTemplates) |> txt ]
    |> Component.Notification.create language `Warning
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ Pool_common.Utils.field_to_string language Pool_message.Field.Filter
          |> CCString.capitalize_ascii
          |> txt
        ]
    ; hint
    ; div ~a:[ a_class [ "gap" ] ] [ list context filter_list query ]
    ]
;;

let edit
      ({ Pool_context.language; csrf; _ } as context)
      filter
      key_list
      query_experiments
      query_tags
  =
  let changelog =
    match filter with
    | None -> txt ""
    | Some filter ->
      let url =
        Http_utils.Url.Admin.filter_path ~suffix:"changelog" ~id:filter.Filter.id ()
        |> Uri.of_string
      in
      Component.Changelog.list context url None
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Component.Partials.form_title language Pool_message.Field.Filter filter
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ Component.Filter.(
            filter_form
              csrf
              language
              (Http_utils.Filter.Template filter)
              key_list
              []
              query_experiments
              query_tags)
        ; changelog
        ]
    ]
;;
