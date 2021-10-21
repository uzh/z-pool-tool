open Tyxml.Html

let index message () =
  let html = h1 [ txt "Welcome to Pool Tool" ] in
  Page_layout.create html message ()
;;
