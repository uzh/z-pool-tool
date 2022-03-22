module HttpUtils = Http_utils
open Tyxml.Html

let htmx_attributes name version ?action () =
  let hx_params = [ "_csrf"; "version"; "field" ] in
  [ a_user_data "hx-swap" "outerHTML"
  ; a_user_data "hx-params" (CCString.concat ", " (CCList.cons name hx_params))
  ; a_user_data "hx-target" "closest div"
  ; a_user_data
      "hx-vals"
      (Format.asprintf
         {|{"version": "%i", "field": "%s"}|}
         (version |> Pool_common.Version.value)
         name)
  ]
  @ CCOption.(CCList.filter_map CCFun.id [ action >|= a_user_data "hx-post" ])
;;

let language_select
    options
    selected
    ?(field = Pool_common.Message.Language)
    ?(attributes = [])
    ()
  =
  let name = Pool_common.Message.field_name field in
  select
    ~a:([ a_name name ] @ attributes)
    (CCList.map
       (fun l ->
         let is_selected =
           selected
           |> CCOption.map (fun selected ->
                  if Pool_common.Language.equal selected l
                  then [ a_selected () ]
                  else [])
           |> CCOption.value ~default:[]
         in
         option
           ~a:([ a_value (Pool_common.Language.code l) ] @ is_selected)
           (txt (Pool_common.Language.code l)))
       options)
;;

let csrf_attibs ?id csrf =
  let attribs = [ a_input_type `Hidden; a_name "_csrf"; a_value csrf ] in
  match id with
  | Some id -> a_id id :: attribs
  | None -> attribs
;;

let csrf_element csrf ?id = input ~a:(csrf_attibs ?id csrf)

let input_element language input_type name input_label value =
  let input_label = Pool_common.Utils.field_to_string language input_label in
  let base_attributes =
    [ a_input_type input_type; a_value value; a_class [ "input" ] ]
  in
  let attributes =
    match name with
    | Some name -> base_attributes @ [ a_name name; a_placeholder input_label ]
    | None -> base_attributes
  in
  match input_type with
  | `Hidden -> input ~a:attributes ()
  | _ ->
    div
      ~a:[ a_class [ "flex-box"; "flex--column" ] ]
      [ label ~a:[ a_class [ "label" ] ] [ txt input_label ]
      ; input ~a:attributes ()
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
  let input_label = Pool_common.Utils.field_to_string language input_label in
  let input =
    textarea
      ~a:([ a_name name; a_class ([ "input" ] @ classnames) ] @ attributes)
      (txt value)
  in
  div
    ~a:[ a_class [ "flex-box"; "flex--column" ] ]
    [ label ~a:[ a_class [ "label" ] ] [ txt input_label ]; input ]
;;

let submit_element lang submit ?(classnames = []) () =
  input
    ~a:
      [ a_input_type `Submit
      ; a_value (Pool_common.Utils.control_to_string lang submit)
      ; a_class ([ "button" ] @ classnames)
      ]
    ()
;;
