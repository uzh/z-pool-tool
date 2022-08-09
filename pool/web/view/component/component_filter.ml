open Tyxml.Html
open Filter
module Input = Component_input

let form_action ?path id =
  let base =
    Format.asprintf
      "/admin/experiments/%s/filter/create"
      (id |> Pool_common.Id.value)
  in
  CCOption.map_or
    ~default:base
    (fun path -> Format.asprintf "%s/%s" base path)
    path
  |> Sihl.Web.externalize_path
;;

let format_identifiers ?prefix identifiers =
  let ids =
    (CCList.fold_left (fun str n ->
         if CCString.is_empty str
         then CCInt.to_string n
         else Format.asprintf "%s-%s" str (CCInt.to_string n)))
      ""
      identifiers
  in
  match prefix with
  | None -> ids
  | Some prefix -> Format.asprintf "%s-%s" prefix ids
;;

let filter_lit csrf experiment =
  div
    [ h3 ~a:[ a_class [ "heading-3" ] ] [ txt "Filter contacts" ]
    ; div
        ~a:[ a_class [ "gap" ] ]
        [ Unsafe.node
            "contact-filter"
            ~a:
              [ a_user_data "action" (form_action experiment.Experiment.id)
              ; a_user_data "csrf" csrf
              ]
            []
        ]
    ]
;;

let select_default_option language selected =
  let attrs = if selected then [ a_selected () ] else [] in
  option
    ~a:attrs
    (txt
       Pool_common.(
         Utils.control_to_string language Message.PleaseSelect
         |> CCString.capitalize_ascii))
;;

let operators_select operators ?selected () =
  let format label =
    CCString.replace ~sub:"_" ~by:" " label |> CCString.capitalize_ascii
  in
  div
    ~a:[ a_class [ "form-group" ] ]
    [ label [ txt "Operator" ]
    ; div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:[ a_name "operator" ]
            (CCList.map
               (fun operator_option ->
                 let selected_attr =
                   CCOption.map_or
                     ~default:[]
                     (fun selected ->
                       if Operator.equal selected operator_option
                       then [ a_selected () ]
                       else [])
                     selected
                 in
                 let str = Operator.to_string operator_option in
                 option ~a:([ a_value str ] @ selected_attr) (txt (format str)))
               operators)
        ]
    ]
;;

let value_input language key ?(value : 'a val' option) () =
  let input_type = key |> Filter.Key.type_of_key in
  match input_type with
  | `Str ->
    let value =
      CCOption.bind value (fun value ->
          match[@warning "-4"] value with
          | Str s -> Some s
          | _ -> None)
    in
    Component_input.input_element
      language
      ?value
      `Text
      Pool_common.Message.Field.Value
  | `Nr ->
    let value =
      CCOption.bind value (fun value ->
          match[@warning "-4"] value with
          | Nr n -> Some n
          | _ -> None)
      |> CCOption.map CCFloat.to_string
    in
    Component_input.input_element
      language
      `Number
      ?value
      Pool_common.Message.Field.Name
  | `Bool ->
    let value =
      CCOption.map_or
        ~default:false
        (fun value ->
          match[@warning "-4"] value with
          | Bool b -> b
          | _ -> false)
        value
    in
    Component_input.checkbox_element
      language
      ~value
      Pool_common.Message.Field.Name
  | `Date ->
    let value =
      CCOption.bind value (fun value ->
          match[@warning "-4"] value with
          | Date d -> Some (d |> Ptime.to_rfc3339)
          | _ -> None)
    in
    Component_input.flatpicker_element
      language
      `Datetime_local
      ?value
      Pool_common.Message.Field.Name
;;

let predicate_toggled language key ?value ?operator () =
  let input_type = key |> Filter.Key.type_of_key in
  let operators = Filter.Utils.input_type_to_operator input_type in
  let operator_select = operators_select operators ?selected:operator () in
  let input_field = value_input language key ?value () in
  div
    ~a:[ a_class [ "switcher-sm"; "flex-gap" ] ]
    [ operator_select; input_field ]
;;

let single_predicate_form language identifier ?key ?operator ?value () =
  let toggle_id = format_identifiers ~prefix:"pred-s" identifier in
  let fields =
    let format k =
      k
      |> Key.show
      |> CCString.replace ~sub:"_" ~by:" "
      |> CCString.capitalize_ascii
    in
    Key.all
    |> CCList.map (fun opt ->
           let selected =
             CCOption.map_or
               ~default:[]
               (fun key -> if Key.equal key opt then [ a_selected () ] else [])
               key
           in
           option
             ~a:([ a_value (Key.show opt) ] @ selected)
             (opt |> format |> txt))
  in
  let toggled_content =
    match key with
    | None -> div []
    | Some key -> predicate_toggled language key ?value ?operator ()
  in
  div
    ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
    [ div
        ~a:[ a_class [ "form-group"; "grow" ] ]
        [ label [ txt "Key" ]
        ; div
            ~a:[ a_class [ "select" ] ]
            [ select
                ~a:
                  [ a_name
                      Pool_common.(
                        Utils.field_to_string language Message.Field.Key)
                  ; a_user_data
                      "hx-post"
                      (Sihl.Web.externalize_path "/admin/filter/toggle-key")
                  ; a_user_data "hx-trigger" "change"
                  ; a_user_data "hx-target" (Format.asprintf "#%s" toggle_id)
                  ]
                fields
            ]
        ]
    ; div ~a:[ a_id toggle_id; a_class [ "grow-2" ] ] [ toggled_content ]
    ]
;;

let predicate_type_select language target identifier ?selected () =
  div
    ~a:[ a_class [ "form-group" ] ]
    [ label [ txt "Type of predicate" ]
    ; div
        ~a:[ a_class [ "select" ] ]
        [ select
            ~a:
              [ a_name Pool_common.Message.Field.(show Predicate)
              ; a_user_data
                  "hx-post"
                  (Sihl.Web.externalize_path
                     "/admin/filter/toggle-predicate-type")
              ; a_user_data "hx-trigger" "change"
              ; a_user_data "hx-target" (Format.asprintf "#%s" target)
              ; a_user_data
                  "hx-vals"
                  (Format.asprintf
                     "{\"id\": \"%s\"}"
                     (format_identifiers identifier))
              ; a_user_data "hx-swap" "outerHTML"
              ]
            (select_default_option language (CCOption.is_none selected)
            :: CCList.map
                 (fun filter_label ->
                   let key, label = Utils.stringify_label filter_label in
                   let selected =
                     CCOption.map_or
                       ~default:[]
                       (fun selected ->
                         if Filter.Utils.equal_filter_label
                              selected
                              filter_label
                         then [ a_selected () ]
                         else [])
                       selected
                   in
                   option ~a:([ a_value key ] @ selected) (txt label))
                 Utils.all_filter_labels)
        ]
    ]
;;

type filter_param =
  | Existing of Filter.filter
  | New of Filter.Utils.filter_label

let rec filter_form
    language
    (filter_param : filter_param)
    ?(identifier = [ 0 ])
    ()
  =
  let selected =
    match filter_param with
    | Existing f ->
      (match f with
      | And _ -> Utils.And
      | Or _ -> Utils.Or
      | Not _ -> Utils.Not
      | PredS _ -> Utils.PredS
      | PredM _ -> Utils.PredM)
    | New f -> f
  in
  let predicate_form =
    match filter_param with
    | New label ->
      let open Filter.Utils in
      (match label with
      | And | Or ->
        CCList.map
          (fun i ->
            filter_form language (New PredS) ~identifier:(identifier @ [ i ]) ())
          [ 0; 1 ]
        |> div ~a:[ a_class [ "stack" ] ]
      | Not ->
        filter_form language (New PredS) ~identifier:(identifier @ [ 0 ]) ()
      | PredS | PredM -> single_predicate_form language identifier ())
    | Existing filter ->
      (match filter with
      | And (f1, f2) | Or (f1, f2) ->
        CCList.mapi
          (fun i filter ->
            filter_form
              language
              (Existing filter)
              ~identifier:(identifier @ [ i ])
              ())
          [ f1; f2 ]
        |> div ~a:[ a_class [ "stack" ] ]
      | Not filter ->
        filter_form
          language
          (Existing filter)
          ~identifier:(identifier @ [ 0 ])
          ()
      | PredS predicate | PredM predicate ->
        let k, o, v = predicate in
        single_predicate_form language identifier ~key:k ~operator:o ~value:v ())
  in
  let predicate_identifier = format_identifiers ~prefix:"filter" identifier in
  div
    ~a:
      [ a_class [ "stack"; "inset"; "border"; "predicate" ]
      ; a_id predicate_identifier
      ]
    [ predicate_type_select
        language
        predicate_identifier
        identifier
        ~selected
        ()
    ; div [ predicate_form ]
    ]
;;
