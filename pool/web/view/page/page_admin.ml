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
module OrganisationalUnit = Page_admin_organisational_units
module Session = Page_admin_session
module Settings = Page_admin_settings
module WaitingList = Page_admin_waiting_list
include Page_admin_edit

let dashboard statistics Pool_context.{ language; _ } =
  let open Pool_common in
  let heading_2 title =
    h2
      ~a:[ a_class [ "heading-2" ] ]
      [ txt (Utils.text_to_string language title) ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.DashboardTitle) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ div
            [ heading_2 I18n.UpcomingSessionsTitle
            ; Component.Calendar.(create User)
            ]
        ; div
            [ heading_2 I18n.PoolStatistics
            ; Component.Statistics.create language statistics
            ]
        ]
    ]
;;
