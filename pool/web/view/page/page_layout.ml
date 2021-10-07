open Tyxml.Html

module Message = struct
  let concat_messages txts styles =
    div ~a:[ a_style styles ] [ txt (CCString.unlines txts) ]
  ;;

  let match_message message classname =
    match message with
    | [] -> div []
    | txts -> concat_messages txts classname
  ;;

  let create message () =
    match message with
    | None -> div []
    | Some message ->
      let success =
        match_message (Http_utils.Message.get_success message) "color: green;"
      in
      let info =
        match_message (Http_utils.Message.get_info message) "color: blue;"
      in
      let warning =
        match_message (Http_utils.Message.get_warning message) "color: orange;"
      in
      let error =
        match_message (Http_utils.Message.get_error message) "color: red;"
      in
      div [ success; info; warning; error ]
  ;;
end

let create children message () =
  let page_title = title (txt "Pool tool") in
  let charset = meta ~a:[ a_charset "utf8" ] () in
  let viewport =
    meta
      ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
      ()
  in
  let message = Message.create message () in
  let content = main [ message; children ] in
  html (head page_title [ charset; viewport ]) (body [ content ])
;;
