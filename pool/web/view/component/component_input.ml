module HttpUtils = Http_utils
module Field = Pool_common.Message.Field
module Icon = Component_icon
open Tyxml.Html

let language_select
  options
  selected
  ?(field = Field.Language)
  ?(attributes = [])
  ()
  =
  let open Pool_common in
  let name = Message.Field.show field in
  let options =
    CCList.map
      (fun l ->
        let is_selected =
          selected
          |> CCOption.map (fun selected ->
               if Language.equal selected l then [ a_selected () ] else [])
          |> CCOption.value ~default:[]
        in
        option
          ~a:([ a_value (Language.show l) ] @ is_selected)
          (txt (Language.show l)))
      options
  in
  div
    ~a:[ a_class [ "select" ] ]
    [ select ~a:([ a_name name ] @ attributes) options ]
;;

let csrf_attibs ?id csrf =
  let attribs = [ a_input_type `Hidden; a_name "_csrf"; a_value csrf ] in
  match id with
  | Some id -> a_id id :: attribs
  | None -> attribs
;;

module Elements = struct
  let input_label language name label_field required =
    let base =
      CCOption.value ~default:name label_field
      |> Pool_common.Utils.field_to_string language
      |> CCString.capitalize_ascii
    in
    if required then Format.asprintf "%s *" base else base
  ;;

  let attributes input_type name id additional_attributes =
    let base_attributes = [ a_input_type input_type; a_id id ] in
    additional_attributes @ base_attributes @ [ a_name (name |> Field.show) ]
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
          [ txt Pool_common.(Utils.hint_to_string language help) ]
      ]
  ;;

  let error language = function
    | None -> []
    | Some error ->
      [ span
          ~a:[ a_class [ "help"; "error-message" ] ]
          [ txt (error |> Pool_common.(Utils.error_to_string language)) ]
      ]
  ;;

  let apply_orientation attributes = function
    | `Vertical -> input ~a:attributes ()
    | `Horizontal ->
      div ~a:[ a_class [ "input-group" ] ] [ input ~a:attributes () ]
  ;;

  let identifier ?identifier name =
    CCOption.value identifier ~default:(Field.show name)
    |> CCString.replace ~which:`All ~sub:" " ~by:"_"
  ;;
end

let flash_fetched_value fetcher value name =
  let old_value =
    CCOption.bind fetcher (fun flash_fetcher ->
      name |> Field.show |> flash_fetcher)
  in
  let open CCOption.Infix in
  old_value <+> value |> CCOption.get_or ~default:""
;;

let csrf_element csrf = input ~a:(csrf_attibs csrf)

let input_element
  ?(orientation = `Vertical)
  ?(classnames = [])
  ?label_field
  ?help
  ?identifier
  ?(required = false)
  ?flash_fetcher
  ?value
  ?error
  ?(additional_attributes = [])
  language
  input_type
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let value = flash_fetched_value flash_fetcher value name in
  let id = Elements.identifier ?identifier name in
  let attributes =
    let attrs =
      Elements.attributes input_type name id [ a_value value ]
      @ additional_attributes
    in
    let attrs = if required then a_required () :: attrs else attrs in
    if CCOption.is_some error then a_class [ "has-error" ] :: attrs else attrs
  in
  match input_type with
  | `Hidden -> input ~a:attributes ()
  | _ ->
    let group_class = Elements.group_class classnames orientation in
    let help = Elements.help language help in
    let error = Elements.error language error in
    let input_element = Elements.apply_orientation attributes orientation in
    div
      ~a:[ a_class group_class ]
      ([ label ~a:[ a_label_for id ] [ txt input_label ]; input_element ]
      @ help
      @ error)
;;

let flatpicker_element
  ?(orientation = `Vertical)
  ?(classnames = [])
  ?label_field
  ?help
  ?identifier
  ?(required = false)
  ?flash_fetcher
  ?value
  ?(warn_past = false)
  ?(disable_past = false)
  ?(additional_attributes = [])
  language
  input_type
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let value = flash_fetched_value flash_fetcher value name in
  let flat_picker_help =
    span ~a:[ a_class [ "help"; "datepicker-msg"; "error-message" ] ] []
    |> CCList.pure
  in
  let input_classes =
    match input_type with
    | `Datetime_local | `Datetime -> [ "datepicker" ]
    | `Time -> [ "spanpicker" ]
  in
  let flatpicker_attributes =
    [ a_class input_classes
    ; a_user_data "language" (Pool_common.Language.show language)
    ]
    @ additional_attributes
    @ CCList.filter_map
        (fun (flag, key, value) ->
          if flag then Some (a_user_data key value) else None)
        [ ( warn_past
          , "warn-past"
          , Pool_common.(Utils.hint_to_string language I18n.SelectedDateIsPast)
          )
        ; disable_past, "disable-past", "true"
        ]
  in
  let id = Elements.identifier ?identifier name in
  let attributes =
    Elements.attributes
      input_type
      name
      id
      ([ a_value value ] @ flatpicker_attributes)
    |> fun attrs -> if required then attrs @ [ a_required () ] else attrs
  in
  let group_class = Elements.group_class classnames orientation in
  let help = Elements.help language help in
  let input_element = Elements.apply_orientation attributes orientation in
  div
    ~a:[ a_class group_class ]
    ([ label ~a:[ a_label_for id ] [ txt input_label ]; input_element ]
    @ help
    @ flat_picker_help)
;;

let checkbox_element
  ?(as_switch = false)
  ?(orientation = `Vertical)
  ?(classnames = [])
  ?label_field
  ?help
  ?error
  ?identifier
  ?flash_fetcher
  ?(required = false)
  ?(value = false)
  ?(additional_attributes = [])
  language
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let value =
    CCOption.bind flash_fetcher (fun flash_fetcher ->
      name |> Field.show |> flash_fetcher)
    |> CCOption.map (fun s -> CCString.equal "true" s)
    |> CCOption.value ~default:value
  in
  let value_attrs =
    (* TODO: Test *)
    match value with
    | true -> [ a_value "true"; a_checked () ]
    | false -> [ a_value "true" ]
  in
  let id = Elements.identifier ?identifier name in
  let attributes =
    Elements.attributes `Checkbox name id value_attrs
    |> fun attrs ->
    if required then CCList.cons (a_required ()) attrs else attrs
  in
  let attributes = attributes @ additional_attributes in
  let group_class = Elements.group_class classnames orientation in
  let help = Elements.help language help in
  let error = Elements.error language error in
  let input_element =
    let checkbox =
      let base = input ~a:attributes () in
      if as_switch
      then
        div
          [ label
              ~a:[ a_class [ "switch" ] ]
              [ base; span ~a:[ a_class [ "slider" ] ] [] ]
          ]
      else base
    in
    match orientation with
    | `Vertical ->
      div [ checkbox; label ~a:[ a_label_for id ] [ txt input_label ] ]
    | `Horizontal ->
      div
        ~a:[ a_class [ "input-group" ] ]
        [ label ~a:[ a_label_for id ] [ txt input_label ]; checkbox ]
  in
  div ~a:[ a_class group_class ] ([ input_element ] @ help @ error)
;;

let input_element_file
  ?(orientation = `Vertical)
  ?(allow_multiple = false)
  ?(has_icon = true)
  ?(required = false)
  ?label_field
  language
  field
  =
  let input_label = Elements.input_label language field label_field required in
  let name = Field.(field |> show) in
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
      span
        ~a:[ a_class [ "has-icon" ] ]
        [ Icon.icon `UploadOutline; placeholder ]
  in
  let input_attributes =
    let attributes =
      [ a_input_type `File
      ; a_id name
      ; a_name name
      ; (if allow_multiple then a_multiple () else a_value "")
      ]
    in
    match required with
    | true -> attributes @ [ a_required () ]
    | false -> attributes
  in
  div
    ~a:[ a_class (Elements.group_class [] orientation) ]
    [ label ~a:[ a_label_for name ] [ txt input_label ]
    ; label
        ~a:[ a_label_for name; a_class [ "file-input" ] ]
        [ input ~a:input_attributes (); visible_part ]
    ]
;;

let textarea_element
  ?(orientation = `Vertical)
  ?(classnames = [])
  ?(attributes = [])
  ?(required = false)
  ?label_field
  ?value
  ?flash_fetcher
  language
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let textarea_attributes =
    let base = [ a_name (name |> Field.show) ] in
    match required with
    | true -> base @ [ a_required () ]
    | false -> base
  in
  let ( <+> ) = CCOption.( <+> ) in
  let old_value =
    CCOption.bind flash_fetcher (fun flash_fetcher ->
      name |> Field.show |> flash_fetcher)
  in
  let value = old_value <+> value |> CCOption.get_or ~default:"" in
  let textarea =
    let base = textarea ~a:(textarea_attributes @ attributes) (txt value) in
    match orientation with
    | `Vertical -> base
    | `Horizontal -> div ~a:[ a_class [ "input-group" ] ] [ base ]
  in
  div
    ~a:[ a_class (Elements.group_class [] orientation @ classnames) ]
    [ label [ txt input_label ]; textarea ]
;;

let submit_element
  lang
  control
  ?(submit_type = `Primary)
  ?(classnames = [])
  ?has_icon
  ?(attributes = [])
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
      (fun i -> [ Component_icon.icon i; text_content ])
      has_icon
  in
  button
    ~a:
      ([ a_button_type `Submit; a_class (classnames @ button_type_class) ]
      @ attributes)
    content
;;

let submit_icon ?(classnames = []) ?(attributes = []) icon_type =
  button
    ~a:
      ([ a_button_type `Submit; a_class (classnames @ [ "has-icon" ]) ]
      @ attributes)
    [ Icon.icon icon_type ]
;;

let selector
  language
  field
  show
  options
  selected
  ?flash_fetcher
  ?(required = false)
  ?help
  ?option_formatter
  ?(attributes = [])
  ?(classnames = [])
  ?(add_empty = false)
  ()
  =
  let name = Field.(show field) in
  let input_label = Elements.input_label language field None required in
  let selected =
    let open CCOption in
    bind flash_fetcher (fun flash_fetcher ->
      field |> Field.show |> flash_fetcher)
    <+> map show selected
  in
  let options =
    CCList.map
      (fun l ->
        let is_selected =
          CCOption.map
            (fun flash ->
              if CCString.equal flash (show l) then [ a_selected () ] else [])
            selected
          |> CCOption.value ~default:[]
        in
        option
          ~a:((l |> show |> a_value) :: is_selected)
          (l
          |> CCOption.value ~default:show option_formatter
          |> CCString.capitalize_ascii
          |> txt))
      options
  in
  let options =
    match add_empty with
    | true ->
      let base_attr = a_value "" in
      let attrs =
        if CCOption.is_none selected
        then [ a_selected (); base_attr ]
        else [ base_attr ]
      in
      let attrs =
        if required then [ a_disabled (); a_hidden () ] @ attrs else attrs
      in
      let default =
        option
          ~a:attrs
          (txt
             (Pool_common.(
                Utils.control_to_string language Message.PleaseSelect)
             |> CCString.capitalize_ascii))
      in
      [ default ] @ options
    | false -> options
  in
  let help = Elements.help language help in
  div
    ~a:[ a_class (Elements.group_class classnames `Vertical) ]
    [ label [ input_label |> txt ]
    ; div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:
              ((a_name name :: attributes)
              @ if required then [ a_required () ] else [])
            options
        ]
    ; div help
    ]
;;

type 'a multi_select =
  { options : 'a list
  ; selected : 'a list
  ; to_label : 'a -> string
  ; to_value : 'a -> string
  }

let multi_select
  language
  { options; selected; to_label; to_value }
  group_field
  ?additional_attributes
  ?(classnames = [])
  ?error
  ?(disabled = false)
  ()
  =
  let error = Elements.error language error in
  CCList.map
    (fun option ->
      let value = to_value option in
      let is_checked =
        CCList.mem
          ~eq:(fun o1 o2 -> CCString.equal (to_value o1) (to_value o2))
          option
          selected
      in
      let input_elm =
        let checked = if is_checked then [ a_checked () ] else [] in
        let disabled = if disabled then [ a_disabled () ] else [] in
        input
          ~a:
            ([ a_input_type `Checkbox
             ; a_name (group_field |> Field.array_key)
             ; a_id value
             ; a_value value
             ]
            @ checked
            @ disabled
            @ CCOption.value ~default:[] additional_attributes)
          ()
      in
      let label = label ~a:[ a_label_for value ] [ txt (option |> to_label) ] in
      div [ input_elm; label ])
    options
  |> fun options_html ->
  let inputs =
    let group_name = Pool_common.(Message.Field.show group_field) in
    input ~a:[ a_input_type `Hidden; a_name group_name; a_value group_name ] ()
    :: options_html
  in
  div
    ~a:[ a_class ([ "form-group"; "horizontal" ] @ classnames) ]
    [ label [ txt Pool_common.(Utils.field_to_string language group_field) ]
    ; div ~a:[ a_class [ "input-group" ] ] (inputs @ error)
    ]
;;
