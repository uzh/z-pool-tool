open Tyxml.Html
module Admins = Page_admin_admins
module Assignment = Page_admin_assignments
module Contact = Page_admin_contact
module CustomFields = Page_admin_custom_fields
module CustomFieldOptions = Page_admin_custom_field_options
module CustomFieldGroups = Page_admin_custom_field_groups
module Experiments = Page_admin_experiments
module Filter = Page_admin_filter
module I18n = Page_admin_i18n
module Location = Page_admin_location
module Mailing = Page_admin_mailings
module MessageTemplate = Page_admin_message_template
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
