module I18n = Page_admin_i18n
open Tyxml.Html
module Settings = Page_admin_settings

let dashboard message () =
  let html = div [ h1 [ txt "Admin Dashboard" ] ] in
  Page_layout.create html message ()
;;
