open Tyxml.Html

let index ~message () =
  let html = div [ h1 [ txt "Welcome" ] ] in
  Page_layout.create ~children:html ~message ()
;;
