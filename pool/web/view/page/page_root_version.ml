open Tyxml.Html
open CCFun.Infix
open Pool_message
module Input = Component.Input

let version_path = Http_utils.Url.Root.version_path
let field_to_string = Pool_common.Utils.field_to_string_capitalized

let list { Pool_context.language; _ } (versions, query) =
  let open Pool_version in
  let url = version_path () |> Uri.of_string in
  let data_table =
    Component.DataTable.create_meta ~search:searchable_by url query language
  in
  let create_btn =
    let open Component in
    version_path ~suffix:"new" ()
    |> Input.link_as_button
         ~style:`Success
         ~icon:Icon.Add
         ~control:(language, Control.Add None)
  in
  let custom field = `custom (txt (field_to_string language field)) in
  let cols =
    [ `column column_tag
    ; custom Field.PublishedAt
    ; custom Field.CreatedAt
    ; `custom create_btn
    ]
  in
  let row ({ id; tag; published_at; created_at; _ } : t) =
    let open CCOption in
    let edit_btn =
      let open Component in
      version_path ~id ~suffix:"edit" ()
      |> Input.link_as_button ~style:`Primary ~icon:Icon.Create
    in
    let format_time = Utils.Ptime.formatted_date_time in
    [ txt (Tag.value tag)
    ; published_at
      |> map_or ~default:"" CCFun.(PublishedAt.value %> format_time)
      |> txt
    ; created_at |> Pool_common.CreatedAt.value |> format_time |> txt
    ; edit_btn
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"versions-list"
    ~cols
    ~row
    data_table
    versions
;;

let index (Pool_context.{ language; _ } as context) versions =
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.VersionsListTitle) ]
    ; list context versions
    ]
;;

let form { Pool_context.csrf; language; _ } ?version ?flash_fetcher () =
  let open Pool_version in
  let open CCOption.Infix in
  let action =
    match version with
    | None -> version_path ()
    | Some version -> version_path ~id:version.id ()
  in
  let control =
    let field = Some Field.Version in
    match CCOption.is_some version with
    | false -> Control.Create field
    | true -> Control.Update field
  in
  let title =
    let to_string = Pool_common.Utils.control_to_string language in
    match version with
    | None -> to_string control
    | Some { tag; _ } ->
      Format.asprintf "%s %s" (to_string control) (Tag.value tag)
  in
  let version_input =
    match version with
    | None ->
      Input.input_element ?flash_fetcher ~required:true language `Text Field.Tag
    | Some _ -> txt ""
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt title ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack" ]
          ; a_user_data "detect-unsaved-changes" ""
          ]
        Input.
          [ csrf_element csrf ()
          ; div
              ~a:[ a_class [ "grid-col-2" ] ]
              [ version_input
              ; Input.textarea_element
                  ~classnames:[ "break"; "full-width" ]
                  ?value:(version >|= fun { text; _ } -> Text.value text)
                  ?flash_fetcher
                  ~rich_text:true
                  ~required:true
                  language
                  Field.Text
              ; div
                  ~a:[ a_class [ "full-width"; "flexrow"; "justify-end" ] ]
                  [ submit_element language control () ]
              ]
          ]
    ]
;;
