open Tyxml.Html
module Admins = Page_admin_admins
module Assignment = Page_admin_assignments
module Contact = Page_admin_contact
module CustomFields = Page_admin_custom_fields
module Experiments = Page_admin_experiments
module I18n = Page_admin_i18n
module Location = Page_admin_location
module Mailing = Page_admin_mailings
module Session = Page_admin_session
module Settings = Page_admin_settings
module WaitingList = Page_admin_waiting_list

let dashboard Pool_context.{ language; _ } =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ]
;;
