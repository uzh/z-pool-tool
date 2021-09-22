open Tyxml.Html

let note title info ~message =
  let html = div [ h1 [ txt title ]; p [ txt info ] ] in
  Page_layout.create ~children:html ~message ()
;;

let error_page_not_found () =
  note
    "Page not found"
    "An error occurred. The requested page could not be found."
    ~message:None
;;
