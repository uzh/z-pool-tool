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

let header =
  header
    ~a:[ a_style "text-align: right; padding: 1rem;" ]
    [ h1 ~a:[ a_style "margin: 0;" ] [ txt "Pool Tool" ] ]
;;

let footer () =
  let title = "Pool Tool" in
  let version =
    Sihl.Configuration.read_string "VERSION"
    |> Option.value ~default:"unknown"
    |> Format.asprintf "Version: %s"
  in
  footer
    ~a:[ a_style "text-align: center; padding: 1rem;" ]
    [ p [ txt @@ CCString.concat " | " [ title; version ] ] ]
;;

let create children message ?(lang = Pool_common.Language.En) () =
  let page_title = title (txt "Pool tool") in
  let charset = meta ~a:[ a_charset "utf8" ] () in
  let viewport =
    meta
      ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
      ()
  in
  let custom_stylesheet =
    link
      ~rel:[ `Stylesheet ]
      ~href:(Sihl.Web.externalize_path "/custom/assets/index.css")
      ()
  in
  let favicon =
    link
      ~rel:[ `Icon ]
      ~href:(Sihl.Web.externalize_path "/custom/assets/favicon")
      ()
  in
  let global_stylesheet =
    link
      ~rel:[ `Stylesheet ]
      ~href:(Sihl.Web.externalize_path "/assets/index.css")
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
       [ charset; viewport; custom_stylesheet; favicon; global_stylesheet ])
    (body [ header; content; footer (); scripts ])
;;
