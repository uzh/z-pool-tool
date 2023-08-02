module Admin = Admin_admins
module Contacts = Admin_contacts
module CustomField = Admin_custom_fields
module CustomFieldGroup = Admin_custom_field_groups
module CustomFieldOption = Admin_custom_field_options
module Experiments = Admin_experiments
module Filter = Admin_filter
module I18n = Admin_i18n
module Location = Admin_location
module Message = Http_utils.Message
module MessageTemplate = Admin_message_templates
module OrganisationalUnit = Admin_organisational_units
module Profile = Admin_user_profile
module Session = Admin_session
module Settings = Admin_settings

let src = Logs.Src.create "handler.admin"
let create_layout req = General.create_tenant_layout req

let statistics_from_request req database_label =
  let open CCOption.Infix in
  let period =
    Sihl.Web.Request.query Pool_common.Message.Field.(show Period) req
    >>= Statistics.read_period
  in
  let%lwt statistics = Statistics.create ?period database_label () in
  Lwt.return (period, statistics)
;;

let dashboard req =
  let result ({ Pool_context.database_label; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/error")
    @@
    let%lwt statistics = statistics_from_request req database_label in
    Page.Admin.dashboard statistics context
    |> create_layout req ~active_navigation:"/admin/dashboard" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path ~src req
;;

(* TODO: Access *)
let statistics req =
  let result { Pool_context.database_label; language; _ } =
    let%lwt statistics = statistics_from_request req database_label in
    Component.Statistics.create language statistics
    |> Http_utils.Htmx.html_to_plain_text_response
    |> Lwt.return_ok
  in
  result
  |> Http_utils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;
