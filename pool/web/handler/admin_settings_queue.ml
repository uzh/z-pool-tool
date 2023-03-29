open CCFun
open Utils.Lwt_result.Infix
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let active_navigation = "/admin/settings/queue"

let show req =
  let result ({ Pool_context.database_label; _ } as context) =
    Pool_tenant.Service.Queue.search
      ~ctx:(Pool_database.to_ctx database_label)
      ()
    ||> fst %> Page.Admin.Settings.Queue.index context
    >|> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, "/"
  in
  result |> HttpUtils.extract_happy_path req
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
  result |> HttpUtils.extract_happy_path req
;;

module Access : Helpers.AccessSig = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let read_effects =
    Guard.(ValidationSet.One (Action.Read, TargetSpec.Entity `Queue))
  ;;

  let index = Guardian.validate_admin_entity read_effects
  let read = Guardian.validate_admin_entity read_effects
end
