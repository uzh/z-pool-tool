module HttpUtils = Http_utils
module Message = Pool_message
module Url = Page.Admin.CustomFields.Url

let src = Logs.Src.create "handler.admin.custom_field_settings"
let create_layout req = General.create_tenant_layout req
let settings_path = "/admin/custom-fields/settings"

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/custom-fields")
    @@
    let%lwt contact_fields = Custom_field.(find_by_model database_label Model.Contact) in
    Page.Admin.CustomFieldSettings.show context contact_fields
    |> create_layout ~active_navigation:settings_path req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let update setting req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/custom-fields")
    @@
    let open Custom_field in
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt selected =
      Sihl.Web.Request.urlencoded_list Pool_message.Field.(array_key CustomField) req
    in
    let%lwt contact_fields = find_by_model database_label Model.Contact in
    let events =
      Cqrs_command.Custom_field_settings_command.UpdateVisibilitySettings.handle
        ~tags
        ~selected
        setting
        contact_fields
        ()
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        settings_path
        [ HttpUtils.Message.set
            ~success:[ Pool_message.(Success.Updated Field.CustomField) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let update_close_screen = update `Close
let update_detail_screen = update `Detail

module Access : module type of Helpers.Access = struct
  module Command = Cqrs_command.Custom_field_settings_command
  module Guardian = Middleware.Guardian
  include Helpers.Access

  let index = Command.UpdateVisibilitySettings.effects |> Guardian.validate_admin_entity
  let update = Command.UpdateVisibilitySettings.effects |> Guardian.validate_admin_entity
end
