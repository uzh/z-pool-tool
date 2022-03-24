open Tyxml.Html

module Message = struct
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
    ~a:[ a_class [ "site-header"; "flex-box"; "flex--row"; "flex--between" ] ]
    [ h1 ~a:[ a_style "margin: 0;" ] [ txt title ]
    ; div [ txt (Pool_common.Language.code lang) ]
    ]
;;

let footer title =
  footer
    ~a:[ a_class [ "site-footer"; "flex-box"; "flex--row"; "flex--center" ] ]
    [ p [ txt title ] ]
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
  let stylesheets = [ global_stylesheet; custom_stylesheet ] in
  let scripts =
    script
      ~a:[ a_src (Sihl.Web.externalize_path "/assets/index.js"); a_defer () ]
      (txt "")
  in
  let content = main ~a:[ a_class [ "site-main" ] ] [ message; children ] in
  html
    (head page_title ([ charset; viewport; favicon ] @ stylesheets))
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
  let content = main ~a:[ a_class [ "site-main" ] ] [ message; children ] in
  html
    (head page_title [ charset; viewport; favicon; global_stylesheet ])
    (body [ header title_text lang; content; footer title_text; scripts ])
;;
