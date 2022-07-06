let combine_html language html_title content =
  let open Tyxml.Html in
  let email_header =
    let pool_title = "Pool Tool" in
    head
      (title
         (txt
            ((CCOption.map_or ~default:pool_title (fun title ->
                  CCString.concat " - " [ title; pool_title ]))
               html_title)))
      [ meta
          ~a:
            [ a_http_equiv "Content-Type"
            ; a_content "text/html; charset=UTF-8"
            ]
          ()
      ; meta
          ~a:
            [ a_name "viewport"
            ; a_content "width=device-width, initial-scale=1"
            ]
          ()
      ; meta ~a:[ a_http_equiv "X-UA-Compatible"; a_content "IE=edge" ] ()
      ; style ~a:[ a_mime_type "text/css" ] []
      ]
  in
  let email_body content =
    body
      ~a:[ a_style "margin:0; padding:0;" ]
      [ div
          ~a:[ a_style "margin: 1em 1em 1em 1em; max-width: 50em;" ]
          [ section ~a:[ a_style "margin-bottom: 1em;" ] [ p [ txt "Logo" ] ]
          ; section ~a:[ a_style "padding-top: 1em; color: #383838;" ] content
          ; footer
              ~a:[ a_style "margin-top: 4em;" ]
              [ div
                  ~a:[ a_style "text-align:center" ]
                  [ small [ txt "Copyright" ] ]
              ]
          ]
      ]
  in
  html
    ~a:[ a_lang (Pool_common.Language.show language) ]
    email_header
    (email_body content)
;;

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ~indent:true ()) html
  |> CCString.replace
       ~which:`Left
       ~sub:"<!DOCTYPE html>"
       ~by:
         {|<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">|}
;;
