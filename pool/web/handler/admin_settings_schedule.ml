module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.settings_schedule"
let active_navigation = "/admin/settings/schedules"

let show req =
  Http_utils.Htmx.handler
    ~active_navigation
    ~error_path:active_navigation
    ~query:(module Schedule)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun context query ->
  let%lwt schedules, query =
    Schedule.find_by_db_label context.Pool_context.database_label query
  in
  let open Page.Admin.Settings.Schedule in
  (if HttpUtils.Htmx.is_hx_request req then list else index) context schedules query
  |> Lwt_result.return
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Schedule.Guard.Access.index |> Guardian.validate_admin_entity
end
