module I18n = Page_admin_i18n
open Tyxml.Html
module Settings = Page_admin_settings
module Experiments = Page_admin_experiments

let dashboard Pool_context.{ language; _ } =
  div
    [ h1 [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ]
;;
