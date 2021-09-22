open Tyxml

let csrf_element csrf =
  [%html {|<input type="hidden" name="_csrf" value="|} csrf {|">|}]
;;
