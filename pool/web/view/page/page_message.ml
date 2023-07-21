open Tyxml.Html

let concat_messages txts classnames =
  div
    ~a:[ a_class ("notification" :: classnames) ]
    [ txt (txts |> CCString.unlines |> CCString.capitalize_ascii)
    ; Component.Icon.(to_html ~classnames:[ "notification-close" ] Close)
    ]
;;

let match_message message classname =
  match message with
  | [] -> None
  | txts -> Some (concat_messages txts classname)
;;

let create ?(attributes = []) message lang () =
  let open Http_utils.Message in
  match message with
  | None -> txt ""
  | Some message ->
    let success = match_message (get_success message lang) [ "success" ] in
    let info = match_message (get_info message lang) [ "neutral" ] in
    let warning = match_message (get_warning message lang) [ "warning" ] in
    let error = match_message (get_error message lang) [ "error" ] in
    [ success; info; warning; error ]
    |> CCList.filter_map (fun notification ->
      match notification with
      | None -> None
      | Some notification ->
        Some
          (div
             ~a:([ a_class [ "notification-fixed"; "fade-out" ] ] @ attributes)
             [ notification ]))
    |> div
;;
