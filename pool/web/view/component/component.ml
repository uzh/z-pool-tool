module HttpUtils = Http_utils
open Tyxml.Html

let language_select
    options
    selected
    ?(field = Pool_common.Message.Field.Language)
    ?(attributes = [])
    ()
  =
  let open Pool_common in
  let name = Message.Field.show field in
  select
    ~a:([ a_name name ] @ attributes)
    (CCList.map
       (fun l ->
         let is_selected =
           selected
           |> CCOption.map (fun selected ->
                  if Language.equal selected l then [ a_selected () ] else [])
           |> CCOption.value ~default:[]
         in
         option
           ~a:([ a_value (Language.code l) ] @ is_selected)
           (txt (Language.code l)))
       options)
;;

let csrf_attibs ?id csrf =
  let attribs = [ a_input_type `Hidden; a_name "_csrf"; a_value csrf ] in
  match id with
  | Some id -> a_id id :: attribs
  | None -> attribs
;;

let csrf_element csrf ?id = input ~a:(csrf_attibs ?id csrf)

(* TODO [aerben] add way to provide additional attributes (min for numbers,
   required) *)
(* TODO [aerben] make this value arg optional *)
let input_element ?info language input_type name value =
  let input_label =
    Pool_common.Utils.field_to_string language name
    |> CCString.capitalize_ascii
    |> fun n ->
    match info with
    | Some info -> Format.asprintf "%s (%s)" n info
    | None -> Format.asprintf "%s" n
  in
  let additional_classes =
    match input_type with
    | `Datetime -> [ "datepicker" ]
    | `Time -> [ "spanpicker" ]
    | _ -> []
  in
  let base_attributes =
    [ a_input_type input_type
    ; a_value value
    ; a_class @@ [ "input" ] @ additional_classes
    ]
  in
  let attributes =
    base_attributes @ [ a_name (name |> Pool_common.Message.Field.show) ]
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
