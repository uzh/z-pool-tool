open Tyxml.Html

let concat_messages txts styles =
  div ~a:[ a_style styles ] [ txt (CCString.unlines txts) ]
;;

let match_message message classname =
  match message with
  | [] -> div []
  | txts -> concat_messages txts classname
;;

let create message lang () =
  let open Http_utils.Message in
  match message with
  | None -> div []
  | Some message ->
    let success = match_message (get_success message lang) "color: green;" in
    let info = match_message (get_info message lang) "color: blue;" in
    let warning = match_message (get_warning message lang) "color: orange;" in
    let error = match_message (get_error message lang) "color: red;" in
    div [ success; info; warning; error ]
;;
