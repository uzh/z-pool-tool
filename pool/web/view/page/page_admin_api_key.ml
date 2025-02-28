open Tyxml.Html
open CCFun.Infix
open Pool_message
module Icon = Component.Icon
module Input = Component.Input

let api_key_path = Http_utils.Url.Admin.api_key_path
let field_to_string = Pool_common.Utils.field_to_string_capitalized

module Partials = struct
  let roles_list ?is_edit ?top_element ({ Pool_context.language; _ } as context) target_id
    =
    let open Component.Role in
    List.create ?is_edit ~path:(Http_utils.Url.Admin.api_key_path ()) context target_id
    %> CCList.return
    %> roles_section ?top_element language
  ;;
end

let list { Pool_context.language; csrf; _ } (api_keys, query) =
  let open Api_key in
  let make_btn = Component.Input.link_as_button in
  let url = api_key_path () |> Uri.of_string in
  let data_table = Component.DataTable.create_meta url query language in
  let create_btn =
    api_key_path ~suffix:"new" ()
    |> make_btn ~style:`Success ~icon:Icon.Add ~control:(language, Control.Add None)
  in
  let cols =
    [ `column column_name
    ; `custom (txt Pool_common.(Utils.field_to_string language Field.ExpiresAt))
    ; `column column_created_at
    ; `custom create_btn
    ]
  in
  let row ({ id; name; expires_at; created_at; _ } : t) =
    let detai_btn = api_key_path ~id () |> make_btn ~icon:Icon.Eye in
    let is_expired =
      Ptime.is_earlier (ExpiresAt.value expires_at) ~than:(Ptime_clock.now ())
    in
    let classname = if is_expired then [ "bg-red-lighter" ] else [] in
    let edit_btn =
      api_key_path ~id ~suffix:"edit" () |> make_btn ~style:`Primary ~icon:Icon.Create
    in
    let disable_form =
      match is_expired with
      | true -> txt ""
      | false ->
        let confirmable =
          Pool_common.(Utils.confirmable_to_string language I18n.DisableApiKey)
        in
        form
          ~a:
            [ a_method `Post
            ; a_action (Sihl.Web.externalize_path (api_key_path ~id ~suffix:"disable" ()))
            ; a_user_data "confirmable" confirmable
            ]
          [ Input.csrf_element csrf ()
          ; button
              ~a:[ a_button_type `Submit; a_class [ "btn"; "error" ] ]
              [ Icon.(CloseCircle |> to_html) ]
          ]
    in
    let format_time = Time.formatted_date_time in
    [ Name.value name |> txt
    ; ExpiresAt.value expires_at |> Time.formatted_date_time |> txt
    ; created_at |> Pool_common.CreatedAt.value |> format_time |> txt
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap-sm"; "justify-end" ] ]
        [ detai_btn; edit_btn; disable_form ]
    ]
    |> CCList.map (CCList.return %> td)
    |> tr ~a:[ a_class classname ]
  in
  Component.DataTable.make ~cols ~row ~target_id:"api-key-list" data_table api_keys
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

let form { Pool_context.csrf; language; _ } ?flash_fetcher ~control ?api_key () =
  let open Api_key in
  let open CCOption.Infix in
  let action =
    match api_key with
    | None -> api_key_path ()
    | Some api_key -> api_key_path ~id:api_key.id ()
  in
  let expires_at =
    match api_key with
    | None ->
      Input.date_time_picker_element
        ~disable_past:true
        ~required:true
        language
        Field.ExpiresAt
    | Some { expires_at; _ } ->
      div
        ~a:[ a_class [ "form-group" ] ]
        [ label [ txt (field_to_string language Field.ExpiresAt) ]
        ; p [ txt (expires_at |> ExpiresAt.value |> Time.formatted_date_time) ]
        ]
  in
  form
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
          ; expires_at
          ; div
              ~a:[ a_class [ "full-width"; "flexrow"; "justify-end" ] ]
              [ submit_element language control () ]
          ]
      ]
;;

let create ({ Pool_context.language; _ } as context) ?flash_fetcher ?api_key () =
  let control = Control.Create (Some Field.ApiKey) in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt (Pool_common.Utils.control_to_string language control) ]
    ; form context ?flash_fetcher ~control ?api_key ()
    ]
;;

let edit
      ({ Pool_context.language; csrf; _ } as context)
      ?flash_fetcher
      api_key
      target_id
      available_roles
      granted_roles
  =
  let open Api_key in
  let control = Control.Edit (Some Field.ApiKey) in
  let roles_form =
    Component.Role.Search.input_form
      ~path:(Http_utils.Url.Admin.api_key_path ())
      csrf
      language
      (Guard.Uuid.target_of Id.value api_key.id)
      available_roles
      ()
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
    [ h1 [ txt (Pool_common.Utils.control_to_string language control) ]
    ; form context ?flash_fetcher ~control ~api_key ()
    ; Partials.roles_list
        ~is_edit:true
        ~top_element:[ roles_form ]
        context
        target_id
        granted_roles
    ]
;;

let show
      ({ Pool_context.language; _ } as context)
      { Api_key.name; token; expires_at; created_at; updated_at; _ }
      target_id
      granted_roles
  =
  let open Api_key in
  let format_ptime = Time.formatted_date_time in
  let details =
    let open Pool_common in
    [ Field.Token, Token.value token |> txt
    ; Field.ExpiresAt, ExpiresAt.value expires_at |> format_ptime |> txt
    ; Field.CreatedAt, CreatedAt.value created_at |> format_ptime |> txt
    ; Field.UpdatedAt, UpdatedAt.value updated_at |> format_ptime |> txt
    ]
    |> Component.Table.vertical_table `Striped language
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "stack-lg" ] ]
    [ h1 [ txt (Name.value name) ]
    ; details
    ; Partials.roles_list ~is_edit:false context target_id granted_roles
    ]
;;
