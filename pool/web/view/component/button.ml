open Tyxml.Html

let add label path =
  let open Message_template in
  a
    ~a:[ a_class [ "btn"; "primary" ]; a_href (Sihl.Web.externalize_path path) ]
    [ txt (Format.asprintf "Add %s" (Label.to_human label)) ]
;;
