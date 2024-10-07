open Tyxml.Html
open CCFun.Infix
open Pool_message
module Icon = Component.Icon

let api_key_path = Http_utils.Url.Admin.api_key_path
let field_to_string = Pool_common.Utils.field_to_string_capitalized

let list { Pool_context.language; _ } (api_keys, query) =
  let open Api_key in
  let make_btn = Component.Input.link_as_button in
  let url = api_key_path () |> Uri.of_string in
  let data_table = Component.DataTable.create_meta url query language in
  let create_btn =
    api_key_path ~suffix:"new" ()
    |> make_btn
         ~style:`Success
         ~icon:Icon.Add
         ~control:(language, Control.Add None)
  in
  let cols =
    [ `column column_name; `column column_created_at; `custom create_btn ]
  in
  let row ({ id; name; created_at; _ } : t) =
    let detai_btn = api_key_path ~id () |> make_btn ~icon:Icon.Eye in
    let edit_btn =
      api_key_path ~id ~suffix:"edit" ()
      |> make_btn ~style:`Primary ~icon:Icon.Create
    in
    let format_time = Utils.Ptime.formatted_date_time in
    [ Name.value name |> txt
    ; created_at |> Pool_common.CreatedAt.value |> format_time |> txt
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm"; "justify-end" ] ]
        [ detai_btn; edit_btn ]
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~cols
    ~row
    ~target_id:"api-key-list"
    data_table
    api_keys
;;

let index (Pool_context.{ language; _ } as context) api_keys =
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.ApiKeys) ]
    ; list context api_keys
    ]
;;

let form { Pool_context.csrf; language; _ } ?flash_fetcher ?api_key () =
  let open Component in
  let open Api_key in
  let open CCOption.Infix in
  let action =
    match api_key with
    | None -> api_key_path ()
    | Some api_key -> api_key_path ~id:api_key.id ()
  in
  let title =
    let field = Some Field.ApiKey in
    match CCOption.is_some api_key with
    | false -> Control.Create field
    | true -> Control.Update field
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt (Pool_common.Utils.control_to_string language title) ]
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
              [ input_element
                  ?flash_fetcher
                  ?value:(api_key >|= fun { name; _ } -> Name.value name)
                  language
                  `Text
                  Field.Name
              ; div
                  ~a:[ a_class [ "full-width"; "flexrow"; "justify-end" ] ]
                  [ submit_element language title () ]
              ]
          ]
    ]
;;

let show
  { Pool_context.language; _ }
  { Api_key.name; token; created_at; updated_at; _ }
  =
  let open Api_key in
  let format_ptime = Utils.Ptime.formatted_date_time in
  let details =
    let open Pool_common in
    [ Field.Token, Token.value token |> txt
    ; Field.CreatedAt, CreatedAt.value created_at |> format_ptime |> txt
    ; Field.UpdatedAt, UpdatedAt.value updated_at |> format_ptime |> txt
    ]
    |> Component.Table.vertical_table `Striped language
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt (Name.value name) ]; details ]
;;
