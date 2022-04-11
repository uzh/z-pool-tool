open Tyxml.Html

let concat_messages txts classnames =
  div ~a:[ a_class classnames ] [ txt (CCString.unlines txts) ]
;;

let match_message message classname =
  match message with
  | [] -> div []
  | txts -> concat_messages txts classname
;;

let create message lang () =
  let open Http_utils.Message in
  let notification_class = "notification" in
  match message with
  | None -> div []
  | Some message ->
    let success =
      match_message
        (get_success message lang)
        [ notification_class; "notification--success" ]
    in
    let info =
      match_message
        (get_info message lang)
        [ notification_class; "notification--neutral" ]
    in
    let warning =
      match_message
        (get_warning message lang)
        [ notification_class; "notification--warning" ]
    in
    let error =
      match_message
        (get_error message lang)
        [ notification_class; "notification--failure " ]
    in
    div [ success; info; warning; error ]
;;
