open Containers
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input
module Field = Pool_message.Field

let tags_path ?suffix () =
  let default = "/admin/settings/tags/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

let layout language children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    (h1
       ~a:[ a_class [ "heading-1"; "has-gap" ] ]
       [ txt Pool_common.(Utils.nav_link_to_string language I18n.Tags) ]
     :: children)
;;

module List = struct
  let row ({ Tags.title; description; model; _ } as tag) =
    let buttons tag =
      tags_path ~suffix:(tag.Tags.id |> Tags.Id.value) ()
      |> Input.edit_link
      |> CCList.return
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt (Tags.Title.value title)
    ; txt (CCOption.map_or ~default:"" Tags.Description.value description)
    ; txt (Tags.Model.show model |> CCString.capitalize_ascii)
    ; buttons tag
    ]
  ;;

  let create { Pool_context.language; _ } tags =
    let thead =
      let open Input in
      (Field.[ Title; Description; Model ] |> Component.Table.fields_to_txt language)
      @ [ link_as_button
            ~style:`Success
            ~icon:Icon.Add
            ~control:(language, Pool_message.(Control.Add (Some Field.Tag)))
            (tags_path ~suffix:"create" ())
        ]
    in
    CCList.map row tags
    |> Component.Table.horizontal_table `Striped ~thead ~align_last_end:true
  ;;
end

let list Pool_context.{ language; _ } tags query =
  let url = Uri.of_string (tags_path ()) in
  let data_table =
    Component.DataTable.create_meta ~search:Tags.searchable_by url query language
  in
  let cols =
    let create_tag : [ | Html_types.flow5 ] elt =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Component.Icon.Add
        ~control:(language, Pool_message.(Control.Add (Some Field.Tag)))
        (tags_path ~suffix:"create" ())
    in
    [ `column Tags.column_title
    ; `column Tags.column_description
    ; `column Tags.column_model
    ; `mobile create_tag
    ]
  in
  let th_class = [ "w-4"; "w-4"; "w-2"; "w-2" ] in
  let row (tag : Tags.t) =
    let open Tags in
    let buttons tag =
      tags_path ~suffix:(tag.Tags.id |> Tags.Id.value) ()
      |> Input.edit_link
      |> CCList.return
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt (Title.value tag.title), Some Field.Title
    ; ( txt (CCOption.map_or ~default:"" Tags.Description.value tag.description)
      , Some Field.Description )
    ; txt (Model.show tag.model |> CCString.capitalize_ascii), Some Field.Model
    ; buttons tag, None
    ]
    |> CCList.map (fun (html, field) ->
      td ~a:(Component.Table.data_label_opt language field) [ html ])
    |> tr
  in
  Component.DataTable.make
    ~break_mobile:true
    ~th_class
    ~target_id:"tags-table"
    ~cols
    ~row
    data_table
    tags
;;

let index (Pool_context.{ language; _ } as context) tags query =
  layout
    language
    [ p
        [ Pool_common.(Utils.hint_to_string language I18n.TagsIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; list context tags query
    ]
;;

let tag_form ?flash_fetcher ?tag Pool_context.{ language; csrf; _ } =
  let open Tags in
  let action =
    match tag with
    | None -> tags_path ()
    | Some tag -> tags_path ~suffix:(tag.id |> Id.value) ()
  in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack" ]
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ Input.csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ Input.input_element
            language
            `Text
            Field.Title
            ?value:(CCOption.map (fun m -> m.title |> Title.value) tag)
            ?flash_fetcher
            ~required:true
        ; Input.input_element
            language
            `Text
            Field.Description
            ?value:CCOption.(bind tag (fun m -> m.description) >|= Description.value)
            ?flash_fetcher
        ; Input.selector
            ~add_empty:(CCOption.is_none tag)
            ~required:(CCOption.is_none tag)
            ~read_only:(CCOption.is_some tag)
            ?flash_fetcher
            language
            Field.Model
            Tags.Model.show
            Tags.Model.all
            (CCOption.map (fun m -> m.Tags.model) tag)
            ()
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ div
            ~a:[ a_class [ "push"; "flexrow"; "flex-gap-lg" ] ]
            [ Input.reset_form_button language
            ; Input.submit_element
                language
                Pool_message.Control.(
                  let field = Some Field.Tag in
                  match tag with
                  | None -> Create field
                  | Some _ -> Update field)
                ~submit_type:`Primary
                ()
            ]
        ]
    ]
;;

let edit ?flash_fetcher ({ Pool_context.language; _ } as context) tag =
  let changelog_url =
    HttpUtils.Url.Admin.Settings.tags_path ~id:tag.Tags.id ~suffix:"changelog" ()
    |> Uri.of_string
  in
  [ div
      ~a:[ a_class [ "stack-lg" ] ]
      [ tag_form ?flash_fetcher ~tag context
      ; Component.Changelog.list context changelog_url None
      ]
  ]
  |> layout language
;;

let new_form ?flash_fetcher ({ Pool_context.language; _ } as context) =
  [ div ~a:[ a_class [ "stack-lg" ] ] [ tag_form ?flash_fetcher context ] ]
  |> layout language
;;
