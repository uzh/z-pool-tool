open Tyxml.Html
open Filter
module Input = Component_input

let notification_id = "filter-notification"

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

let form_action = Format.asprintf "/admin/filter/%s"

let htmx_attribs ~action ~trigger ?target ?(swap = "outerHTML") ?identifier () =
  let target =
    target
    |> CCOption.map (fun target ->
         a_user_data "hx-target" (Format.asprintf "#%s" target))
  in
  let identifier =
    identifier
    |> CCOption.map (fun identifier ->
         a_user_data
           "hx-vals"
           (Format.asprintf "{\"id\": \"%s\"}" (format_identifiers identifier)))
  in
  [ a_user_data "hx-post" (action |> Sihl.Web.externalize_path)
  ; a_user_data "hx-trigger" trigger
  ; a_user_data "hx-swap" swap
  ]
  @ CCList.filter_map CCFun.id [ target; identifier ]
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

let operators_select language ?operators ?selected () =
  let format label =
    CCString.replace ~sub:"_" ~by:" " label |> CCString.capitalize_ascii
  in
  match operators with
  | None -> txt ""
  | Some operators ->
    Component_input.selector
      ~option_formatter:CCFun.(Operator.show %> format)
      language
      Pool_common.Message.Field.Operator
      Operator.show
      operators
      selected
      ()
;;

let value_input language input_type ?value () =
  let open Filter in
  let open CCOption.Infix in
  let field_name = Pool_common.Message.Field.Value in
  let single_value =
    value
    >>= function
    | Single s -> Some s
    | Lst _ -> None
  in
  let find_in_options options option_id =
    CCList.find_opt
      Custom_field.(
        fun option -> SelectOption.Id.equal option.SelectOption.id option_id)
      options
  in
  match input_type with
  | None -> div []
  | Some input_type ->
    let additional_attributes =
      [ a_user_data "input-type" Filter.Key.(show_input_type input_type) ]
    in
    (match input_type with
     | Key.Str ->
       let value =
         single_value
         >>= function[@warning "-4"]
         | Str s -> Some s
         | _ -> None
       in
       Component_input.input_element
         ~additional_attributes
         ?value
         language
         `Text
         field_name
     | Key.Nr ->
       let value =
         single_value
         >>= (function[@warning "-4"]
               | Nr n -> Some n
               | _ -> None)
         |> CCOption.map (fun f -> f |> CCFloat.to_int |> CCInt.to_string)
       in
       Component_input.input_element
         ~additional_attributes
         language
         `Number
         ?value
         field_name
     | Key.Bool ->
       let value =
         CCOption.map_or
           ~default:false
           (fun value ->
             match[@warning "-4"] value with
             | Bool b -> b
             | _ -> false)
           single_value
       in
       Component_input.checkbox_element
         ~additional_attributes
         ~as_switch:true
         ~value
         language
         field_name
     | Key.Date ->
       let value =
         single_value
         >>= function[@warning "-4"]
         | Date d -> Some (d |> Ptime.to_rfc3339)
         | _ -> None
       in
       Component_input.flatpicker_element
         ~additional_attributes
         ?value
         language
         `Datetime_local
         field_name
     | Key.Select options ->
       let selected =
         single_value
         >>= function[@warning "-4"]
         | Option o -> find_in_options options o
         | _ -> None
       in
       Component_input.selector
         ~attributes:additional_attributes
         ~option_formatter:(Custom_field.SelectOption.name language)
         language
         field_name
         Custom_field.SelectOption.show_id
         options
         selected
         ()
     | Key.MultiSelect options ->
       let[@warning "-4"] selected =
         CCOption.map_or
           ~default:[]
           (fun value ->
             match value with
             | Single _ -> []
             | Lst lst ->
               CCList.filter_map
                 (fun value ->
                   match value with
                   | Option id -> find_in_options options id
                   | _ -> None)
                 lst)
           value
       in
       let multi_select =
         Component_input.
           { options
           ; selected
           ; to_label = Custom_field.SelectOption.name language
           ; to_value = Custom_field.SelectOption.show_id
           }
       in
       Component_input.multi_select
         ~additional_attributes
         ~orientation:`Vertical
         language
         multi_select
         field_name
         ())
;;

let predicate_value_form language ?key ?value ?operator () =
  let open CCOption.Infix in
  let input_type = key >|= Filter.Key.type_of_key in
  let operators = input_type >|= Filter.Operator.input_type_to_operator in
  let operator_select =
    operators_select language ?operators ?selected:operator ()
  in
  let input_field = value_input language input_type ?value () in
  div
    ~a:[ a_class [ "switcher-sm"; "flex-gap" ] ]
    [ operator_select; input_field ]
;;

let single_predicate_form language identifier key_list ?key ?operator ?value () =
  let toggle_id = format_identifiers ~prefix:"pred-s" identifier in
  let toggled_content =
    predicate_value_form language ?key ?value ?operator ()
  in
  let key_selector =
    let attributes =
      [ a_user_data
          "hx-post"
          (Sihl.Web.externalize_path "/admin/filter/toggle-key")
      ; a_user_data "hx-trigger" "change"
      ; a_user_data "hx-target" (Format.asprintf "#%s" toggle_id)
      ]
    in
    Component_input.selector
      ~attributes
      ~add_empty:true
      ~option_formatter:(Key.human_to_label language)
      ~required:true
      ~classnames:[ "key-select" ]
      language
      Pool_common.Message.Field.Key
      Key.human_to_value
      key_list
      key
      ()
  in
  div
    ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
    [ key_selector
    ; div ~a:[ a_id toggle_id; a_class [ "grow-2" ] ] [ toggled_content ]
    ]
;;

let predicate_type_select language target identifier ?selected () =
  let attributes =
    htmx_attribs
      ~action:(form_action "toggle-predicate-type")
      ~trigger:"change"
      ~target
      ~identifier
      ()
  in
  Component_input.selector
    ~option_formatter:Utils.to_label
    ~attributes
    language
    Pool_common.Message.Field.Predicate
    Utils.show_filter_label
    Utils.all_filter_labels
    selected
    ()
;;

let add_predicate_btn identifier =
  let id = format_identifiers ~prefix:"new" identifier in
  div
    ~a:[ a_id id; a_user_data "new-predicate" "" ]
    [ Input.submit_icon
        ~attributes:
          (htmx_attribs
             ~action:(form_action "add-predicate")
             ~trigger:"click"
             ~target:id
             ~identifier
             ())
        `Add
    ]
;;

let rec predicate_form language filter key_list ?(identifier = [ 0 ]) () =
  let filter = CCOption.value ~default:(Filter.Human.init ()) filter in
  let predicate_identifier = format_identifiers ~prefix:"filter" identifier in
  let selected =
    let open Human in
    match filter with
    | And _ -> Utils.And
    | Or _ -> Utils.Or
    | Not _ -> Utils.Not
    | Pred _ -> Utils.Pred
  in
  let delete_button () =
    div
      ~a:[ a_user_data "delete-predicate" "" ]
      [ Component_icon.icon `TrashOutline ]
  in
  let predicate_form =
    let open Human in
    match filter with
    | filter ->
      (match filter with
       | And filters | Or filters ->
         CCList.mapi
           (fun i filter ->
             let filter = CCOption.pure filter in
             predicate_form
               language
               filter
               key_list
               ~identifier:(identifier @ [ i ])
               ())
           filters
         @ [ add_predicate_btn (identifier @ [ CCList.length filters ]) ]
       | Not filter ->
         predicate_form
           language
           (Some filter)
           key_list
           ~identifier:(identifier @ [ 0 ])
           ()
         |> CCList.pure
       | Pred predicate ->
         let ({ Predicate.key; operator; value } : Predicate.human) =
           predicate
         in
         single_predicate_form
           language
           identifier
           key_list
           ?key
           ?operator
           ?value
           ()
         |> CCList.pure)
  in
  let data_attr = [ a_user_data "predicate" Filter.Human.(show filter) ] in
  div
    ~a:
      ([ a_class [ "stack"; "inset"; "border"; "predicate" ]
       ; a_id predicate_identifier
       ]
      @ data_attr)
    ([ predicate_type_select
         language
         predicate_identifier
         identifier
         ~selected
         ()
     ; div ~a:[ a_class [ "predicate-wrapper"; "stack" ] ] predicate_form
     ]
    @ if CCList.length identifier > 1 then [ delete_button () ] else [])
;;

let filter_form csrf language experiment filter key_list =
  let action =
    Format.asprintf
      "/admin/experiments/%s/filter/create"
      (Pool_common.Id.value experiment.Experiment.id)
  in
  let predicates = predicate_form language filter key_list () in
  div
    ~a:[ a_class [ "stack" ] ]
    [ div
        [ txt "Nr of contacts: "
        ; span
            ~a:
              [ a_id "contact-counter"
              ; a_user_data
                  "experiment-id"
                  (experiment.Experiment.id |> Pool_common.Id.value)
              ]
            []
        ]
    ; div
        ~a:
          [ a_user_data "action" action
          ; a_id "filter-form"
          ; a_class [ "stack" ]
          ]
        [ div ~a:[ a_id notification_id ] []
        ; Component_input.csrf_element csrf ()
        ; predicates
        ; Component_input.submit_element
            language
            ~attributes:
              (a_id "submit-filter-form"
              :: htmx_attribs ~action ~swap:"none" ~trigger:"click" ())
            Pool_common.Message.(Save None)
            ()
        ]
    ]
;;
