open Tyxml.Html
open CCFun.Infix
open Pool_message
module Input = Component.Input

let version_path = Http_utils.Url.Admin.version_path
let field_to_string = Pool_common.Utils.field_to_string_capitalized

let list { Pool_context.language; _ } (versions, query) =
  let open Pool_version in
  let url = version_path () |> Uri.of_string in
  let data_table = Component.DataTable.create_meta url query language in
  let custom field = `custom (txt (field_to_string language field)) in
  let cols = [ `column column_version; custom Field.CreatedAt; `empty ] in
  let row ({ id; version; published_at; _ } : t) =
    let open CCOption in
    let show_btn =
      let open Component in
      version_path ~id () |> Input.link_as_button ~style:`Primary ~icon:Icon.Eye
    in
    let format_time = Utils.Ptime.formatted_date_time in
    [ txt (Version.value version)
    ; published_at
      |> map_or ~default:"" CCFun.(PublishedAt.value %> format_time)
      |> txt
    ; show_btn
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

let show { Pool_context.language; _ } { Pool_version.version; text; _ } =
  let open Pool_version in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Format.asprintf
               "%s %s"
               (Pool_common.Utils.field_to_string_capitalized
                  language
                  Field.Version)
               (Version.value version))
        ]
    ; div [ Unsafe.data (Text.value text) ]
    ]
;;
