open Tyxml.Html

let dashboard message () =
  let html = div [ h1 [ txt "Admin Dashboard" ] ] in
  Page_layout.create html message ()
;;
