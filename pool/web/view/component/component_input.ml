module HttpUtils = Http_utils
module Field = Pool_common.Message.Field
module Icon = Component_icon
open Tyxml.Html

let submit_type_to_class = function
  | `Disabled -> "disabled"
  | `Error -> "error"
  | `Primary -> "primary"
  | `Success -> "success"
;;

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
    if required then Format.asprintf "%s*" base else base
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
      help
      |> Pool_common.Utils.hint_to_string language
      |> Utils.Html.handle_line_breaks (span ~a:[ a_class [ "help" ] ])
      |> CCList.return
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
  ?(disabled = false)
  ?flash_fetcher
  ?value
  ?error
  ?(additional_attributes = [])
  ?(append_html = [])
  language
  input_type
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let value = flash_fetched_value flash_fetcher value name in
  let id = Elements.identifier ?identifier name in
  let attributes =
    let attrs =
      let additional =
        if input_type == `Password then [ a_autocomplete `Off ] else []
      in
      Elements.attributes input_type name id (a_value value :: additional)
      @ additional_attributes
    in
    let attrs = if required then a_required () :: attrs else attrs in
    let attrs = if disabled then a_disabled () :: attrs else attrs in
    if CCOption.is_some error then a_class [ "is-invalid" ] :: attrs else attrs
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
       @ error
       @ append_html)
;;

let flatpicker_element
  input_type
  ?(append_html = [])
  ?(orientation = `Vertical)
  ?(classnames = [])
  ?error
  ?label_field
  ?help
  ?identifier
  ?(required = false)
  ?flash_fetcher
  ?value
  ?(warn_past = false)
  ?(disable_past = false)
  ?(disable_future = false)
  ?(additional_attributes = [])
  ?(success = false)
  language
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let value = flash_fetched_value flash_fetcher value name in
  let flat_picker_help =
    span ~a:[ a_class [ "help"; "datepicker-msg"; "error-message" ] ] []
    |> CCList.pure
  in
  let disable_hours =
    match input_type with
    | `Date -> true
    | `DateTime -> false
  in
  let flatpicker_attributes =
    let input_class =
      "datepicker" :: (if success then [ "is-valid" ] else [])
    in
    [ a_class input_class
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
        ; disable_hours, "disable-time", "true"
        ; disable_future, "disable-future", "true"
        ]
  in
  let id = Elements.identifier ?identifier name in
  let attributes =
    Elements.attributes
      `Datetime_local
      name
      id
      ([ a_value value ] @ flatpicker_attributes)
    |> fun attrs -> if required then attrs @ [ a_required () ] else attrs
  in
  let group_class = Elements.group_class classnames orientation in
  let help = Elements.help language help in
  let error = Elements.error language error in
  let input_element = Elements.apply_orientation attributes orientation in
  div
    ~a:[ a_class group_class ]
    ([ label ~a:[ a_label_for id ] [ txt input_label ]; input_element ]
     @ help
     @ error
     @ flat_picker_help
     @ append_html)
;;

let date_picker_element ?value =
  let value =
    value
    |> CCOption.map (fun date -> Pool_common.Model.Ptime.date_to_flatpickr date)
  in
  flatpicker_element `Date ?value
;;

let date_time_picker_element ?value =
  let value =
    value |> CCOption.map Pool_common.Model.Ptime.date_time_to_flatpickr
  in
  flatpicker_element `DateTime ?value
;;

let timespan_picker
  ?(additional_attributes = [])
  ?(orientation = `Vertical)
  ?(classnames = [])
  ?help
  ?identifier
  ?label_field
  ?(required = false)
  ?flash_fetcher
  ?value
  ?error
  language
  name
  =
  let human_field = CCOption.value ~default:name label_field in
  let input_label = Elements.input_label language human_field None required in
  let value =
    flash_fetched_value
      flash_fetcher
      (value |> CCOption.map Pool_common.Utils.Time.timespan_to_minutes)
      name
  in
  let id = Elements.identifier ?identifier human_field in
  let attributes =
    let attrs =
      Elements.attributes `Number name id [ a_value value ]
      @ additional_attributes
      @ [ a_input_min (`Number 0); a_step (Some 1.) ]
    in
    let attrs = if required then a_required () :: attrs else attrs in
    if CCOption.is_some error then a_class [ "is-invalid" ] :: attrs else attrs
  in
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

let checkbox_element
  ?(additional_attributes = [])
  ?(as_switch = false)
  ?(switcher_class = [])
  ?(classnames = [])
  ?error
  ?flash_fetcher
  ?help
  ?identifier
  ?label_field
  ?(orientation = `Vertical)
  ?(required = false)
  ?(disabled = false)
  ?(value = false)
  ?(append_html = [])
  language
  name
  =
  let input_label = Elements.input_label language name label_field required in
  let value =
    CCOption.bind flash_fetcher (fun flash_fetcher ->
      name |> Field.show |> flash_fetcher)
    |> CCOption.map (CCString.equal "true")
    |> CCOption.value ~default:value
  in
  let value_attrs =
    match value with
    | true -> [ a_value "true"; a_checked () ]
    | false -> [ a_value "true" ]
  in
  let id = Elements.identifier ?identifier name in
  let attributes =
    Elements.attributes `Checkbox name id value_attrs
    |> fun attrs ->
    if required && not as_switch
    then CCList.cons (a_required ()) attrs
    else attrs
  in
  let attributes =
    if disabled then a_disabled () :: attributes else attributes
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
              ~a:[ a_class ("switch" :: switcher_class) ]
              [ base; span ~a:[ a_class [ "slider" ] ] [] ]
          ]
      else base
    in
    match orientation with
    | `Vertical ->
      (match as_switch with
       | true -> [ label ~a:[ a_label_for id ] [ txt input_label ]; checkbox ]
       | false ->
         [ div [ checkbox; label ~a:[ a_label_for id ] [ txt input_label ] ] ])
    | `Horizontal ->
      [ label ~a:[ a_label_for id ] [ txt input_label ]
      ; div ~a:[ a_class [ "input-group" ] ] [ checkbox ]
      ]
  in
  div ~a:[ a_class group_class ] (input_element @ help @ error @ append_html)
;;

let input_element_file
  ?(orientation = `Vertical)
  ?(allow_multiple = false)
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
        ~a:[ a_class [ "file-placeholder" ] ]
        [ txt
            Pool_common.(
              Utils.control_to_string language Message.SelectFilePlaceholder)
        ]
    in
    span
      ~a:[ a_class [ "has-icon" ] ]
      [ Icon.(to_html UploadOutline)
      ; span ~a:[ a_class [ "file-name" ] ] []
      ; placeholder
      ]
  in
  let input_attributes =
    let attributes = [ a_input_type `File; a_id name; a_name name ] in
    let attributes =
      if allow_multiple then a_multiple () :: attributes else attributes
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
  ?(attributes = [])
  ?(classnames = [])
  ?flash_fetcher
  ?(orientation = `Vertical)
  ?label_field
  ?identifier
  ?help
  ?(required = false)
  ?(rich_text = false)
  ?value
  language
  name
  =
  let id = Elements.identifier ?identifier name in
  let input_label = Elements.input_label language name label_field required in
  let ( <+> ) = CCOption.( <+> ) in
  let old_value =
    CCOption.bind flash_fetcher (fun flash_fetcher ->
      name |> Field.show |> flash_fetcher)
  in
  let value = old_value <+> value |> CCOption.get_or ~default:"" in
  let help = Elements.help language help in
  let textarea_attributes =
    let base = [ a_name (name |> Field.show); a_id id ] in
    let base = if rich_text then a_class [ "rich-text" ] :: base else base in
    (* Chrome has problems with CKEditor, when field is required and initially
       empty *)
    match rich_text, required, CCString.(trim value |> length) > 0 with
    | false, true, _ | true, true, true -> base @ [ a_required () ]
    | true, _, _ | _, false, _ -> base
  in
  let textarea =
    let base = textarea ~a:(textarea_attributes @ attributes) (txt value) in
    match orientation with
    | `Vertical -> base
    | `Horizontal -> div ~a:[ a_class [ "input-group" ] ] [ base ]
  in
  div
    ~a:[ a_class (Elements.group_class [] orientation @ classnames) ]
    ([ label ~a:[ a_label_for id ] [ txt input_label ]; textarea ] @ help)
;;

let submit_element
  lang
  control
  ?(is_text = false)
  ?(submit_type = `Primary)
  ?(classnames = [])
  ?has_icon
  ?(attributes = [])
  ()
  =
  let button_type_class =
    (submit_type_to_class submit_type
     :: CCOption.map_or ~default:[] (fun _ -> [ "has-icon" ]) has_icon)
    @ if is_text then [ "is-text" ] else []
  in
  let text_content =
    span [ txt Pool_common.Utils.(control_to_string lang control) ]
  in
  let content =
    CCOption.map_or
      ~default:[ text_content ]
      (fun i -> [ Icon.to_html i; text_content ])
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
    [ Icon.to_html icon_type ]
;;

let link_as_button
  ?(is_text = false)
  ?(style = `Primary)
  ?(classnames = [])
  ?(attributes = [])
  ?icon
  ?control
  href
  =
  let classnames =
    let base =
      (submit_type_to_class style :: "btn" :: classnames)
      @ if is_text then [ "is-text" ] else []
    in
    match icon with
    | None -> base
    | Some _ -> "has-icon" :: base
  in
  let attrs =
    [ a_href (Sihl.Web.externalize_path href); a_class classnames ] @ attributes
  in
  let content =
    let icon_elm =
      match icon with
      | None -> txt ""
      | Some i -> Icon.to_html i
    in
    let control =
      match control with
      | None -> txt ""
      | Some (language, control) ->
        let base =
          Pool_common.Utils.(control_to_string language control)
          |> CCString.capitalize_ascii
          |> txt
        in
        if CCOption.is_some icon then span [ base ] else base
    in
    [ icon_elm; control ]
  in
  a ~a:attrs content
;;

let edit_link ?classnames ?attributes href =
  link_as_button ?classnames ?attributes ~icon:Icon.Create href
;;

let selector
  language
  field
  show
  options
  selected
  ?(add_empty = false)
  ?(attributes = [])
  ?(classnames = [])
  ?(disabled = false)
  ?(hide_label = false)
  ?(required = false)
  ?(read_only = false)
  ?error
  ?flash_fetcher
  ?help
  ?option_formatter
  ?elt_option_formatter
  ?option_disabler
  ?(append_html = [])
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
  let attributes =
    let checks =
      [ CCOption.is_some error, a_class [ "is-invalid" ]
      ; read_only, a_disabled ()
      ; required, a_required ()
      ; disabled, a_disabled ()
      ]
    in
    CCList.fold_left
      (fun attrs (check, attr) -> if check then attr :: attrs else attrs)
      attributes
      checks
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
        let is_disabled =
          option_disabler
          |> CCOption.map_or ~default:false (fun fnc -> fnc l)
          |> function
          | true -> [ a_disabled () ]
          | false -> []
        in
        let attrs = is_selected @ is_disabled in
        let label =
          match elt_option_formatter with
          | Some fnc -> fnc l
          | None ->
            l
            |> CCOption.value ~default:show option_formatter
            |> CCString.capitalize_ascii
            |> txt
        in
        option ~a:((l |> show |> a_value) :: attrs) label)
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
      let attrs = if required then [ a_disabled () ] @ attrs else attrs in
      let default =
        option
          ~a:attrs
          (txt
             Pool_common.(Utils.control_to_string language Message.PleaseSelect))
      in
      [ default ] @ options
    | false -> options
  in
  let hidden_field =
    if read_only
    then
      input
        ~a:
          [ a_input_type `Hidden
          ; a_name name
          ; a_value (CCOption.value ~default:"" selected)
          ]
        ()
    else txt ""
  in
  let help = Elements.help language help in
  let error = Elements.error language error in
  div
    ~a:[ a_class (Elements.group_class classnames `Vertical) ]
    ([ (if hide_label then txt "" else label [ input_label |> txt ])
     ; div
         ~a:[ a_class [ "select" ] ]
         [ select ~a:(a_name name :: attributes) options; hidden_field ]
     ]
     @ help
     @ error
     @ append_html)
;;

let organisational_units_selector langauge units selected =
  let open Organisational_unit in
  selector
    ~add_empty:true
    ~option_formatter:(fun ou -> ou.name |> Name.value)
    langauge
    Field.OrganisationalUnit
    (fun ou -> ou.id |> Id.value)
    units
    selected
    ()
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
  ?(orientation = `Horizontal)
  ?additional_attributes
  ?(classnames = [])
  ?error
  ?(disabled = false)
  ?(required = false)
  ?append_html
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
  |> fun inputs ->
  let classnames =
    if CCOption.is_some append_html
    then classnames @ [ "flexrow"; "wrap" ]
    else classnames
  in
  div
    ~a:[ a_class (Elements.group_class classnames orientation) ]
    [ label [ txt (Elements.input_label language group_field None required) ]
    ; div ~a:[ a_class [ "input-group" ] ] (inputs @ error)
    ; append_html
      |> CCOption.map_or
           ~default:(txt "")
           (div ~a:[ a_class [ "flex-basis-100" ] ])
    ]
;;

let reset_form_button language =
  span
    ~a:
      [ a_class [ "has-icon"; "color-red"; "pointer" ]
      ; a_user_data "reset-form" ""
      ]
    [ Icon.(to_html RefreshOutline)
    ; txt Pool_common.(Utils.control_to_string language Message.(Reset None))
    ]
;;

let cell_phone_input ?(required = false) () =
  let attrs = if required then [ a_required () ] else [] in
  let to_option =
    CCList.map (fun (code, label) ->
      option ~a:[ a_value (CCInt.to_string code) ] (txt label))
  in
  let options = Utils.PhoneCodes.all_human |> to_option in
  div
    ~a:[ a_class [ "flexrow"; "even"; "flex-gap" ] ]
    [ div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:([ a_name Pool_common.Message.Field.(show AreaCode) ] @ attrs)
            options
        ]
    ; div
        ~a:[ a_class [ "form-group" ] ]
        [ input
            ~a:
              ([ a_name Pool_common.Message.Field.(show CellPhone)
               ; a_class [ "input" ]
               ; a_input_type `Number
               ; a_placeholder "791234567"
               ]
               @ attrs)
            ()
        ]
    ]
;;

let notify_via_selection language =
  div
    ~a:[ a_class [ "form-group" ] ]
    (label
       [ Elements.input_label
           language
           Pool_common.Message.Field.NotifyVia
           None
           true
         |> txt
       ]
     :: Pool_common.(
          NotifyVia.all
          |> CCList.map (fun option ->
            let checked =
              NotifyVia.checked_by_default option
              |> function
              | false -> []
              | true -> [ a_checked () ]
            in
            div
              [ input
                  ~a:
                    ([ a_input_type `Checkbox
                     ; a_value (NotifyVia.show option)
                     ; a_id (NotifyVia.show option)
                     ; a_name (Field.array_key Field.NotifyVia)
                     ]
                     @ checked)
                  ()
              ; label
                  ~a:[ a_label_for (NotifyVia.show option) ]
                  [ NotifyVia.to_human language option |> txt ]
              ])))
;;

let admin_select
  language
  options
  selected
  field
  ?(attributes = [])
  ?(required = false)
  ?help
  ()
  =
  let open Pool_common in
  let open Admin in
  let name = Message.Field.show field in
  let select_attrs =
    let name = [ a_name name ] in
    let attrs =
      match required with
      | true -> a_required () :: name
      | false -> name
    in
    attrs @ attributes
  in
  let help = Elements.help language help in
  let options =
    let default_option =
      let attrs = [ a_value "" ] in
      let attrs =
        match selected with
        | Some _ -> attrs
        | None -> a_selected () :: attrs
      in
      let attrs =
        match required with
        | true -> a_disabled () :: attrs
        | false -> attrs
      in
      option
        ~a:attrs
        (Pool_common.(Utils.control_to_string language Message.PleaseSelect)
         |> txt)
    in
    CCList.map
      (fun (admin : Admin.t) ->
        let is_selected =
          selected
          |> CCOption.map (fun selected ->
            if Id.equal (id admin) selected then [ a_selected () ] else [])
          |> CCOption.value ~default:[]
        in
        option
          ~a:([ a_value (admin |> id |> Id.value) ] @ is_selected)
          (txt (full_name admin)))
      options
    |> CCList.cons default_option
  in
  div
    ~a:[ a_class [ "form-group" ] ]
    ([ label
         [ field
           |> Utils.(field_to_string language)
           |> CCString.capitalize_ascii
           |> txt
         ]
     ; div ~a:[ a_class [ "select" ] ] [ select ~a:select_attrs options ]
     ]
     @ help)
;;

let custom_field_to_static_input
  ?(force_required = false)
  ?flash_fetcher
  language
  custom_field
  =
  let open Custom_field in
  let open CCOption in
  let field = Public.to_common_field language custom_field in
  let help = Public.to_common_hint language custom_field in
  let required =
    force_required || Public.required custom_field |> Required.value
  in
  let create input_type value =
    input_element
      ?flash_fetcher
      ?value
      ?help
      ~required
      language
      input_type
      field
  in
  match custom_field with
  | Public.Boolean (_, answer) ->
    checkbox_element
      ~as_switch:true
      ~orientation:`Horizontal
      ?value:(answer >>= Answer.value)
      language
      field
  | Public.Date (_, answer) ->
    date_picker_element
      ~disable_future:true
      ?value:(answer >>= Answer.value)
      ~required
      language
      field
  | Public.MultiSelect (_, options, answer) ->
    let selected = answer >>= Answer.value |> CCOption.value ~default:[] in
    let t =
      { options
      ; selected
      ; to_label = SelectOption.Public.name language
      ; to_value = SelectOption.Public.show_id
      }
    in
    multi_select language t field ()
  | Public.Number (_, answer) ->
    answer >>= Answer.value >|= CCInt.to_string |> create `Number
  | Public.Text (_, answer) -> answer >>= Answer.value |> create `Text
  | Public.Select (_, options, answer) ->
    let value = answer >>= Answer.value in
    selector
      ?flash_fetcher
      ?help
      ~required
      ~option_formatter:SelectOption.Public.(name language)
      ~add_empty:true
      language
      field
      SelectOption.Public.show_id
      options
      value
      ()
;;
