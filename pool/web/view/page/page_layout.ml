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

let add_styles =
  {css|
    input {
      margin-right: 1rem;
      margin-bottom: 1rem;
    }
    @keyframes input-success {
      from {box-shadow: 0px 0px 5px 0px rgba(0,128,0,1);}
      to {box-shadow: 0px 0px 5px 0px rgba(0,128,0,0);}
    }
    input.success {
      animation-name: input-success;
      animation-duration: 2s;
    }
    @keyframes input-error {
      from {box-shadow: 0px 0px 5px 0px rgba(255,0,0,1);}
      to {box-shadow: 0px 0px 5px 0px rgba(255,0,0,0);}
    }
    input.error {
      animation-name: input-error;
      animation-duration: 2s;
    }
    .flex-wrap  {
      display: flex;
      flex-wrap: wrap;
    }
    .flexcolumn {
      display: flex;
      flex-direction: column;
    }
    .error-message {
      color: red;
    }
  |css}
;;

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
  let footer = script ~a:[ a_src "/assets/index.js"; a_defer () ] (txt "") in
  let content = main [ message; children ] in
  html
    (head page_title [ charset; viewport; stylesheet ])
    (body [ content; footer ])
;;
