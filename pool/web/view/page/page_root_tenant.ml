open Tyxml

let list _ ~message () =
  let html = [%html {|
          <h1>Tenants</h1>
          |}] in
  Page_layout.create ~children:[ html ] ~message ()
;;
