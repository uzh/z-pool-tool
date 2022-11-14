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
module Session = Admin_session
module Settings = Admin_settings

let create_layout req = General.create_tenant_layout req

let dashboard req =
  let result context =
    let open Lwt_result.Infix in
    Lwt_result.map_error (fun err -> err, "/error")
    @@ (Page.Admin.dashboard context
       |> create_layout req ~active_navigation:"/admin/dashboard" context
       >|= Sihl.Web.Response.of_html)
  in
  result |> Http_utils.extract_happy_path req
;;
