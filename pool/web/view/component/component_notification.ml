open Tyxml.Html

let style_to_class = function
  | `Default -> ""
  | `Error -> "error"
  | `Success -> "success"
  | `Warning -> "warning"
;;

let notification ?link language style html =
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
    ~a:
      [ a_class
          [ "notification"
          ; style_to_class style
          ; "flexrow"
          ; "wrap"
          ; "flex-gap"
          ; "justify-between"
          ]
      ]
    [ div html; link ]
;;
