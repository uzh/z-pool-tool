module HttpUtils = Http_utils
open Tyxml.Html

let icon icon_type =
  (match icon_type with
  | `CalendarOutline -> "calendar-outline"
  | `Calendar -> "calendar"
  | `CreateOutline -> "create-outline"
  | `Create -> "create"
  | `EarthOutline -> "earth-outline"
  | `Earth -> "earth"
  | `LeafOutline -> "leaf-outline"
  | `Leaf -> "leaf"
  | `LocationOutline -> "location-outline"
  | `Location -> "location"
  | `MailOutline -> "mail-outline"
  | `Mail -> "mail"
  | `PersonOutline -> "person-outline"
  | `Person -> "person"
  | `SaveOutline -> "save-outline"
  | `Save -> "save"
  | `SchoolOutline -> "school-outline"
  | `School -> "school"
  | `TrashOutline -> "trash-outline"
  | `Trash -> "trash"
  | `UploadOutline -> "upload-outline")
  |> fun icon_class ->
  i ~a:[ a_class [ Format.asprintf "icon-%s" icon_class ] ] []
;;

let language_select
    options
    selected
    ?(field = Pool_common.Message.Field.Language)
    ?(attributes = [])
    ()
  =
  let open Pool_common in
  let name = Message.Field.show field in
  div
    ~a:[ a_class [ "select" ] ]
    [ select
        ~a:([ a_name name ] @ attributes)
        (CCList.map
           (fun l ->
             let is_selected =
               selected
               |> CCOption.map (fun selected ->
                      if Language.equal selected l
                      then [ a_selected () ]
                      else [])
               |> CCOption.value ~default:[]
             in
             option
               ~a:([ a_value (Language.show l) ] @ is_selected)
               (txt (Language.show l)))
           options)
    ]
;;

let csrf_attibs ?id csrf =
  let attribs = [ a_input_type `Hidden; a_name "_csrf"; a_value csrf ] in
  match id with
  | Some id -> a_id id :: attribs
  | None -> attribs
;;

module Elements = struct
  let input_label language name label_field info =
    CCOption.value ~default:name label_field
    |> Pool_common.Utils.field_to_string language
    |> CCString.capitalize_ascii
    |> fun n ->
    match info with
    | Some info -> Format.asprintf "%s (%s)" n info
    | None -> Format.asprintf "%s" n
  ;;

  let attributes input_type name additional_attributes =
    let base_attributes = [ a_input_type input_type ] in
    additional_attributes
    @ base_attributes
    @ [ a_name (name |> Pool_common.Message.Field.show) ]
  ;;

  let group_class classnames orientation =
    [ "form-group" ]
    @ classnames
    @
    match orientation with
    | `Vertical -> []
    | `Horizontal -> [ "horizontal"; "flex-gap" ]
  ;;

  let help language = function
    | None -> []
    | Some help ->
      [ span
          ~a:[ a_class [ "help" ] ]
          [ txt Pool_common.(Utils.text_to_string language help) ]
      ]
  ;;

  let apply_orientation attributes = function
    | `Vertical -> input ~a:attributes ()
    | `Horizontal ->
      div ~a:[ a_class [ "input-group" ] ] [ input ~a:attributes () ]
  ;;

  let identifier ?identifier language name =
    CCOption.value
      identifier
      ~default:(Pool_common.Utils.field_to_string language name)
    |> CCString.replace ~which:`All ~sub:" " ~by:"_"
  ;;
end

let csrf_element csrf ?id = input ~a:(csrf_attibs ?id csrf)

(* TODO [aerben] add way to provide additional attributes (min for numbers,
   required) *)
(* TODO [aerben] make this value arg optional *)

let input_element
    ?(orientation = `Vertical)
    ?(classnames = [])
    ?label_field
    ?help
    ?info
    ?identifier
    language
    input_type
    name
    value
  =
  let input_label = Elements.input_label language name label_field info in
  let input_classes =
    match input_type with
    | `Datetime -> [ "datepicker" ]
    | `Time -> [ "spanpicker" ]
    | _ -> []
  in
  let id = Elements.identifier ?identifier language name in
  let attributes =
    Elements.attributes
      input_type
      name
      [ a_value value; a_id id; a_class input_classes ]
  in
  match input_type with
  | `Hidden -> input ~a:attributes ()
  | _ ->
    let group_class = Elements.group_class classnames orientation in
    let help = Elements.help language help in
    let input_element = Elements.apply_orientation attributes orientation in
    div
      ~a:[ a_class group_class ]
      ([ label ~a:[ a_label_for id ] [ txt input_label ]; input_element ] @ help)
;;

let checkbox_element
    ?(orientation = `Vertical)
    ?(classnames = [])
    ?label_field
    ?help
    ?info
    ?identifier
    language
    input_type
    name
    value
  =
  let input_label = Elements.input_label language name label_field info in
  let value_attrs =
    match value with
    | true -> [ a_checked () ]
    | false -> []
  in
  let id = Elements.identifier ?identifier language name in
  let attributes =
    Elements.attributes input_type name (value_attrs @ [ a_id id ])
  in
  match input_type with
  | `Hidden -> input ~a:attributes ()
  | _ ->
    let group_class = Elements.group_class classnames orientation in
    let help = Elements.help language help in
    let input_element = Elements.apply_orientation attributes orientation in
    div
      ~a:[ a_class group_class ]
      [ div
          ([ input_element; label ~a:[ a_label_for id ] [ txt input_label ] ]
          @ help)
      ]
;;

(* Use the value from flash as the value, if no value is in flash use a default
   value *)
let input_element_persistent
    ?info
    ?default
    language
    input_type
    name
    flash_fetcher
  =
  let old_value = name |> Pool_common.Message.Field.show |> flash_fetcher in
  let open CCOption in
  let value = old_value <+> default |> get_or ~default:"" in
  input_element ?info language input_type name value
;;

let input_element_file
    ?(orientation = `Vertical)
    ?(allow_multiple = false)
    ?(has_icon = true)
    language
    field
  =
  let field_label =
    Pool_common.Utils.field_to_string language field
    |> CCString.capitalize_ascii
  in
  let name = Pool_common.Message.Field.(field |> show) in
  let visible_part =
    let placeholder =
      span
        [ txt
            Pool_common.(
              Utils.control_to_string language Message.SelectFilePlaceholder)
        ]
    in
    match has_icon with
    | false -> placeholder
    | true ->
      span ~a:[ a_class [ "has-icon" ] ] [ icon `UploadOutline; placeholder ]
  in
  div
    ~a:[ a_class (Elements.group_class [] orientation) ]
    [ label ~a:[ a_label_for name ] [ txt field_label ]
    ; label
        ~a:[ a_label_for name; a_class [ "file-input" ] ]
        [ input
            ~a:
              [ a_input_type `File
              ; a_id name
              ; a_name name
              ; (if allow_multiple then a_multiple () else a_value "")
              ]
            ()
        ; visible_part
        ]
    ]
;;

let textarea_element
    language
    name
    input_label
    value
    ?(classnames = [])
    ?(attributes = [])
    ()
  =
  let input_label =
    Pool_common.Utils.field_to_string language input_label
    |> CCString.capitalize_ascii
  in
  let input =
    textarea ~a:([ a_name name; a_class classnames ] @ attributes) (txt value)
  in
  div ~a:[ a_class [ "form-group" ] ] [ label [ txt input_label ]; input ]
;;

let submit_element
    lang
    control
    ?(submit_type = `Primary)
    ?(classnames = [])
    ?has_icon
    ()
  =
  let button_type_class =
    (match has_icon with
    | Some _ -> [ "has-icon" ]
    | None -> [])
    @ CCList.pure
    @@
    match submit_type with
    | `Disabled -> "disabled"
    | `Error -> "error"
    | `Primary -> "primary"
    | `Success -> "success"
  in
  let text_content =
    span [ txt Pool_common.Utils.(control_to_string lang control) ]
  in
  let content =
    CCOption.map_or
      ~default:[ text_content ]
      (fun i -> [ icon i; text_content ])
      has_icon
  in
  button
    ~a:[ a_button_type `Submit; a_class (classnames @ button_type_class) ]
    content
;;

let submit_icon ?(classnames = []) icon_type =
  button
    ~a:[ a_button_type `Submit; a_class (classnames @ [ "has-icon" ]) ]
    [ icon icon_type ]
;;

module Table = struct
  let head language fields =
    CCList.map
      (fun field ->
        th
          [ txt
              (CCOption.map_or
                 ~default:""
                 (fun f -> Pool_common.Utils.field_to_string language f)
                 field)
          ])
      fields
    |> tr
    |> CCList.pure
    |> thead
  ;;
end
