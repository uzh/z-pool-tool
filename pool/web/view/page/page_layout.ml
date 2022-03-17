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

let charset = meta ~a:[ a_charset "utf8" ] ()

let viewport =
  meta
    ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
    ()
;;

let favicon =
  link
    ~rel:[ `Icon ]
    ~href:(Sihl.Web.externalize_path "/assets/images/favicon.png")
    ()
;;

let global_stylesheet =
  link
    ~rel:[ `Stylesheet ]
    ~href:(Sihl.Web.externalize_path "/assets/index.css")
    ()
;;

let header title lang =
  header
    ~a:[ a_style "text-align: right; padding: 1rem;" ]
    [ h1 ~a:[ a_style "margin: 0;" ] [ txt title ]
    ; div [ txt (Pool_common.Language.code lang) ]
    ]
;;

let footer title =
  footer ~a:[ a_style "text-align: center; padding: 1rem;" ] [ p [ txt title ] ]
;;

let create children message lang =
  let title_text = "Pool tool" in
  let page_title = title (txt title_text) in
  let custom_stylesheet =
    link
      ~rel:[ `Stylesheet ]
      ~href:(Sihl.Web.externalize_path "/custom/assets/index.css")
      ()
  in
  let message = Message.create message lang () in
  let scripts =
    script
      ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
      (txt "")
  in
  let content = main [ message; children ] in
  html
    (head
       page_title
       [ charset; viewport; custom_stylesheet; global_stylesheet; favicon ])
    (body [ header title_text lang; content; footer title_text; scripts ])
;;

let create_root_layout children message lang =
  let title_text = "Pool tool" in
  let page_title = title (txt title_text) in
  let message = Message.create message lang () in
  let scripts =
    script
      ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
      (txt "")
  in
  let content = main [ message; children ] in
  html
    (head page_title [ charset; viewport; global_stylesheet; favicon ])
    (body [ header title_text lang; content; footer title_text; scripts ])
;;
