open Tyxml.Html

let create ~document_title content =
  let stylesheet = `GlobalStylesheet |> Layout_utils.css_link_tag in
  html (head (title (txt document_title)) [ stylesheet ]) (body content)
;;
