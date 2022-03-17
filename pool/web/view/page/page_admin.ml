module I18n = Page_admin_i18n
open Tyxml.Html
module Settings = Page_admin_settings

let dashboard message Pool_context.{ language; _ } =
  let html = div [ h1 [ txt "Admin Dashboard" ] ] in
  Page_layout.Tenant.create_layout html message language
;;
