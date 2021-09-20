open Tyxml

let index ~message () =
  let html = [%html {|<h1>Welcome</h1>|}] in
  Page_layout.create ~children:[ html ] ~message ()
;;
