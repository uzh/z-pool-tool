open Utils.Lwt_result.Infix
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.settings_queue"
let active_navigation = "/admin/settings/queue"

let show req =
  HttpUtils.Htmx.handler
    ~active_navigation:"/admin/settings/queue"
    ~error_path:"/admin"
    ~create_layout:General.create_tenant_layout
    ~query:(module Queue)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt queue = Queue.find_by ~query database_label in
  let open Page.Admin.Settings.Queue in
  (if HttpUtils.Htmx.is_hx_request req then data_table else index) context queue
  |> Lwt_result.return
;;

let detail req =
  let result ({ Pool_context.database_label; _ } as context) =
    let id = HttpUtils.find_id Pool_common.Id.of_string Field.Queue req in
    Queue.find database_label id
    >|+ Page.Admin.Settings.Queue.detail context
    >>= General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, "/admin/settings/queue"
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Queue.Guard.Access.index |> Guardian.validate_admin_entity
  let read = Queue.Guard.Access.read |> Guardian.validate_admin_entity
end
