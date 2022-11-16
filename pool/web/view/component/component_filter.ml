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

let htmx_attribs
  ~action
  ~trigger
  ?target
  ?(swap = "outerHTML")
  ?(allow_empty_values = false)
  ?identifier
  ()
  =
  let target =
    target
    |> CCOption.map (fun target ->
         a_user_data "hx-target" (Format.asprintf "#%s" target))
  in
  let hx_vals =
    let identifier =
      identifier
      |> CCOption.map (fun identifier -> "id", format_identifiers identifier)
    in
    let allow_empty_values =
      if allow_empty_values then Some ("allow_empty_values", "true") else None
    in
    [ identifier; allow_empty_values ]
    |> CCList.filter_map CCFun.id
    |> fun values ->
    if CCList.is_empty values
    then []
    else
      values
      |> CCList.map (fun (key, value) ->
           Format.asprintf "\"%s\": \"%s\"" key value)
      |> CCString.concat ","
      |> fun values -> [ a_user_data "hx-vals" (Format.asprintf "{%s}" values) ]
  in
  [ a_user_data "hx-post" (action |> Sihl.Web.externalize_path)
  ; a_user_data "hx-trigger" trigger
  ; a_user_data "hx-swap" swap
  ]
  @ hx_vals
  @ CCList.filter_map CCFun.id [ target ]
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
      htmx_attribs
        ~action:"/admin/filter/toggle-key"
        ~trigger:"change"
        ~swap:"innerHTML"
        ~target:toggle_id
        ~allow_empty_values:true
        ()
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
      ~allow_empty_values:true
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
             ~allow_empty_values:true
             ())
        `Add
    ]
;;

let rec predicate_form
  language
  filter
  key_list
  subfilter_list
  ?(identifier = [ 0 ])
  ()
  =
  let filter = CCOption.value ~default:(Filter.Human.init ()) filter in
  let predicate_identifier = format_identifiers ~prefix:"filter" identifier in
  let selected =
    let open Human in
    match filter with
    | And _ -> Utils.And
    | Or _ -> Utils.Or
    | Not _ -> Utils.Not
    | Pred _ -> Utils.Pred
    | SubQuery _ -> Utils.SubQuery
  in
  let delete_button () =
    div
      ~a:[ a_user_data "delete-predicate" "" ]
      [ Component_icon.icon `TrashOutline ]
  in
  let predicate_form =
    let open Human in
    match filter with
    | And queries | Or queries ->
      CCList.mapi
        (fun i query ->
          let query = CCOption.pure query in
          predicate_form
            language
            query
            key_list
            subfilter_list
            ~identifier:(identifier @ [ i ])
            ())
        queries
      @ [ add_predicate_btn (identifier @ [ CCList.length queries ]) ]
    | Not query ->
      predicate_form
        language
        (Some query)
        key_list
        subfilter_list
        ~identifier:(identifier @ [ 0 ])
        ()
      |> CCList.pure
    | Pred predicate ->
      let ({ Predicate.key; operator; value } : Predicate.human) = predicate in
      single_predicate_form
        language
        identifier
        key_list
        ?key
        ?operator
        ?value
        ()
      |> CCList.pure
    | SubQuery id ->
      let selected =
        CCOption.bind id (fun id ->
          subfilter_list
          |> CCList.find_opt (fun filter -> Pool_common.Id.equal filter.id id))
      in
      Component_input.selector
        ~add_empty:true
        ~option_formatter:(fun f ->
          f.title
          |> CCOption.map_or ~default:(f.id |> Pool_common.Id.value) Title.value)
        language
        Pool_common.Message.Field.Subquery
        (fun f -> f.id |> Pool_common.Id.value)
        subfilter_list
        selected
        ()
      |> CCList.pure
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

type form_param =
  | ExperimentParam of Experiment.t
  | FilterParam of Filter.t option

let filter_form csrf language param key_list subfilter_list =
  let filter, action =
    let open Experiment in
    match param with
    | ExperimentParam experiment ->
      ( experiment.filter
      , Format.asprintf
          "/admin/experiments/%s/filter/create"
          (Pool_common.Id.value experiment.Experiment.id) )
    | FilterParam filter ->
      ( filter
      , (match filter with
         | Some filter ->
           Format.asprintf "/admin/filter/%s" (filter.id |> Pool_common.Id.value)
         | None -> "/admin/filter") )
  in
  let filter_query =
    filter
    |> CCOption.map
         Filter.(
           fun filter -> filter.query |> t_to_human key_list subfilter_list)
  in
  let result_counter =
    match param with
    | FilterParam _ -> txt ""
    | ExperimentParam experiment ->
      div
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
  in
  let predicates =
    predicate_form language filter_query key_list subfilter_list ()
  in
  let title_input, _ =
    match param with
    | FilterParam _ ->
      let open CCOption.Infix in
      let open Pool_common.Message in
      ( Component_input.input_element
          ?value:(filter >>= fun filter -> filter.title >|= Title.value)
          ~required:true
          language
          `Text
          Field.Title
      , [ a_user_data "hx-params" Field.(show Title) ] )
    | ExperimentParam _ -> txt "", []
  in
  let filter_id =
    (match param with
     | ExperimentParam experiment ->
       experiment.Experiment.filter |> CCOption.map (fun f -> f.id)
     | FilterParam f -> f |> CCOption.map (fun f -> f.Filter.id))
    |> CCOption.map_or ~default:[] (fun id ->
         Pool_common.[ a_user_data Message.Field.(show filter) (Id.value id) ])
  in
  div
    ~a:[ a_class [ "stack" ] ]
    [ result_counter
    ; div
        ~a:
          ([ a_user_data "action" action
           ; a_id "filter-form"
           ; a_class [ "stack" ]
           ]
          @ filter_id)
        [ div ~a:[ a_id notification_id ] []
        ; Component_input.csrf_element csrf ()
        ; title_input
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
