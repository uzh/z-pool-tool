open Tyxml.Html

let csrf_element csrf =
  input ~a:[ a_input_type `Hidden; a_name "_csrf"; a_value csrf ]
;;
