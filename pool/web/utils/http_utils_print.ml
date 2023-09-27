let html_body ~document_title content =
  let open Tyxml.Html in
  html (head (title (txt document_title)) []) (body content)
;;
