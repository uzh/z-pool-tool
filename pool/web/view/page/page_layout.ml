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
end

let create children message ?(lang = Pool_common.Language.En) () =
  let page_title = title (txt "Pool tool") in
  let charset = meta ~a:[ a_charset "utf8" ] () in
  let viewport =
    meta
      ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
      ()
  in
  let stylesheet =
    link
      ~rel:[ `Stylesheet ]
      ~href:(Sihl.Web.externalize_path "/assets/index.css")
      ()
  in
  let message = Message.create message lang () in
  let content = main [ message; children ] in
  html (head page_title [ charset; viewport; stylesheet ]) (body [ content ])
;;
