open Tyxml.Html

let add ?(is_text = false) label path =
  let classnames = [ "btn"; "primary" ] @ if is_text then [ "is-text" ] else [] in
  a
    ~a:[ a_class classnames; a_href (Sihl.Web.externalize_path path) ]
    [ txt (Format.asprintf "Add %s" (Pool_common.MessageTemplateLabel.to_human label)) ]
;;
