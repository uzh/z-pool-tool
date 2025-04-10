open Tyxml.Html

let style_to_class = function
  | `Default -> ""
  | `Error -> "error"
  | `Success -> "success"
  | `Warning -> "warning"
;;

let create
      ?(attributes = [])
      ?link
      ?(close = txt "")
      ?(classnames = [])
      language
      style
      html
  =
  let classnames =
    [ "notification"; "flexcolumn"; "flex-gap"; "align-stretch"; style_to_class style ]
    @ classnames
  in
  let link =
    match link with
    | None -> txt ""
    | Some (link, label) ->
      div
        ~a:[ a_class [ "push" ] ]
        [ a
            ~a:[ a_href (Sihl.Web.externalize_path link) ]
            [ txt Pool_common.(Utils.nav_link_to_string language label) ]
        ]
  in
  div
    ~a:([ a_class classnames ] @ attributes)
    [ div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between" ] ] [ div html; close ]
    ; link
    ]
;;
