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

module List = struct
  let row { Organisational_unit.id; name } =
    Organisational_unit.
      [ txt (Name.value name); Input.edit_link (ou_path ~id ~suffix:"edit" ()) ]
  ;;

  let create { Pool_context.language; _ } organisational_units =
    let open Pool_common in
    let thead =
      (Message.Field.[ Name ] |> Component.Table.fields_to_txt language)
      @ [ Input.link_as_button
            ~style:`Success
            ~icon:Icon.Add
            ~control:(language, Message.(Add (Some Field.OrganisationalUnit)))
            (ou_path ~suffix:"create" ())
        ]
    in
    CCList.map row organisational_units
    |> Component.Table.horizontal_table `Striped ~thead ~align_last_end:true
  ;;
end

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

let index ({ Pool_context.language; _ } as context) organisational_units =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.nav_link_to_string language I18n.OrganisationalUnits)
        ]
    ; List.create context organisational_units
    ]
;;
