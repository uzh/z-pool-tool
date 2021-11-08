module HttpUtils = Http_utils
open Tyxml.Html

let csrf_element csrf =
  input ~a:[ a_input_type `Hidden; a_name "_csrf"; a_value csrf ]
;;

let input_element input_type name value =
  let base_attributes = [ a_input_type input_type; a_value value ] in
  let attributes =
    match name with
    | Some name ->
      base_attributes
      @ [ a_name name; a_placeholder (HttpUtils.placeholder_from_name name) ]
    | None -> base_attributes
  in
  input ~a:attributes ()
;;
