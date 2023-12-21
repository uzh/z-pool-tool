open CCFun
open Tyxml.Html
open Component

let ou_path ?suffix ?id () =
  let open Organisational_unit in
  let base_path =
    Format.asprintf
      "/admin/%s"
      Pool_common.Message.Field.(human_url OrganisationalUnit)
  in
  let default =
    match id with
    | None -> base_path
    | Some id -> Format.asprintf "%s/%s" base_path (Id.value id)
  in
  CCOption.map_or ~default (Format.asprintf "%s/%s" default) suffix
;;

let form { Pool_context.language; csrf; _ } organisational_unit =
  let open Organisational_unit in
  let open Pool_common in
  let action, control =
    match organisational_unit with
    | None -> ou_path (), Message.(Create (Some Field.OrganisationalUnit))
    | Some ou ->
      ou_path ~id:ou.id (), Message.(Update (Some Field.OrganisationalUnit))
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "haeding-1" ] ]
        [ Utils.control_to_string language control |> txt ]
    ; form
        ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
        [ Input.csrf_element csrf ()
        ; Input.input_element
            ?value:
              (organisational_unit
               |> CCOption.map (fun { name; _ } -> Name.value name))
            language
            `Text
            Pool_common.Message.Field.Name
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ Input.submit_element ~classnames:[ "push" ] language control () ]
        ]
    ]
;;

let list { Pool_context.language; _ } organizations query =
  let open Pool_common in
  let url = Uri.of_string (ou_path ()) in
  let sort = Component.Sortable_table.{ url; query; language } in
  let cols =
    let create_btn : [ | Html_types.flow5 ] elt =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Message.(Add (Some Field.OrganisationalUnit)))
        (ou_path ~suffix:"create" ())
    in
    [ `column Organisational_unit.column_name; `custom create_btn ]
  in
  let row (org : Organisational_unit.t) =
    let open Organisational_unit in
    [ txt (Name.value org.name)
    ; Component.Input.edit_link (ou_path ~id:org.id ~suffix:"edit" ())
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  let target_id = "organisations-table" in
  let open Component in
  div
    ~a:[ a_id target_id ]
    [ List.create
        ~url
        ~target_id
        language
        (Sortable_table.make ~target_id ~cols ~row sort)
        []
        (organizations, query)
    ]
;;

let index ({ Pool_context.language; _ } as context) organizations query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.nav_link_to_string language I18n.OrganisationalUnits)
        ]
    ; list context organizations query
    ]
;;
