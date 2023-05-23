open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.settings_schedule"
let active_navigation = "/admin/settings/schedules"

let show req =
  let result context =
    Schedule.find_all ()
    ||> Page.Admin.Settings.Schedule.index context
    >|> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, "/"
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Schedule.Guard.Access.index |> Guardian.validate_admin_entity
end
