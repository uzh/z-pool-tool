open CCFun
open Tyxml.Html
open Component
open Pool_message
open Control

let ou_path ?suffix ?id () =
  let base_path = Format.asprintf "/admin/%s" Field.(human_url OrganisationalUnit) in
  let default =
    match id with
    | None -> base_path
    | Some id -> Format.asprintf "%s/%s" base_path (Organisational_unit.Id.value id)
  in
  CCOption.map_or ~default (Format.asprintf "%s/%s" default) suffix
;;

let form { Pool_context.language; csrf; flash_fetcher; _ } organisational_unit =
  let open Organisational_unit in
  let open Pool_common in
  let action, control =
    match organisational_unit with
    | None -> ou_path (), Create (Some Field.OrganisationalUnit)
    | Some ou -> ou_path ~id:ou.id (), Update (Some Field.OrganisationalUnit)
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "has-gap" ] ] [ Utils.control_to_string language control |> txt ]
    ; form
        ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
        [ Input.csrf_element csrf ()
        ; Input.input_element
            ?flash_fetcher
            ?value:
              (organisational_unit |> CCOption.map (fun { name; _ } -> Name.value name))
            language
            `Text
            Field.Name
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ Input.submit_element ~classnames:[ "push" ] language control () ]
        ]
    ]
;;

let create context = div ~a:[ a_class [ "trim"; "safety-margin" ] ] [ form context None ]

let detail context ou =
  let open Organisational_unit in
  let url =
    Http_utils.Url.Admin.organisational_unit_path ~suffix:"changelog" ~id:ou.id ()
    |> Uri.of_string
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ form context (Some ou)
    ; div ~a:[ a_class [ "gap-lg" ] ] [ Component.Changelog.list context url None ]
    ]
;;

let list { Pool_context.language; _ } organizations query =
  let open Component in
  let url = Uri.of_string (ou_path ()) in
  let data_table =
    Component.DataTable.create_meta
      ~search:Organisational_unit.searchable_by
      url
      query
      language
  in
  let cols =
    let create_btn : [ | Html_types.flow5 ] elt =
      Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Add (Some Field.OrganisationalUnit))
        (ou_path ~suffix:"create" ())
    in
    [ `column Organisational_unit.column_name; `custom create_btn ]
  in
  let row (org : Organisational_unit.t) =
    let open Organisational_unit in
    [ txt (Name.value org.name); Input.edit_link (ou_path ~id:org.id ~suffix:"edit" ()) ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"organisations-table"
    ~cols
    ~row
    data_table
    organizations
;;

let index ({ Pool_context.language; _ } as context) organizations query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.OrganisationalUnits) ]
    ; list context organizations query
    ]
;;
