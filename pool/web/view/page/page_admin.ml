open Tyxml.Html
module I18n = Page_admin_i18n
module Settings = Page_admin_settings
module Session = Page_admin_session
module Experiments = Page_admin_experiments
module Location = Page_admin_location
module WaitingList = Page_admin_waiting_list
module Assignment = Page_admin_assignments

let dashboard Pool_context.{ language; _ } =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ]
;;
