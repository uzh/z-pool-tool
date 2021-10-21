open Tyxml.Html

let note page_title info message =
  let html = div [ h1 [ txt page_title ]; p [ txt info ] ] in
  Page_layout.create html message ()
;;

let error_page_not_found () =
  note
    "Page not found"
    "An error occurred. The requested page could not be found."
    None
;;
