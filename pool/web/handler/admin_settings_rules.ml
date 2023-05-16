open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let active_navigation = "/admin/settings/rules"

let show req =
  let result ({ Pool_context.database_label; user; _ } as context) =
    let actor = Pool_context.Utils.find_authorizable_opt database_label user in
    actor
    >|> CCOption.map_or
          ~default:(Lwt.return [])
          (Guard.Persistence.Rule.find_all_by_actor database_label)
    ||> CCList.stable_sort Guard.Rule.compare
    ||> Page.Admin.Settings.Rules.index context
    >|> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, "/"
  in
  result |> HttpUtils.extract_happy_path req
;;

module Access = struct
  let index =
    Guard.Access.manage_rules |> Middleware.Guardian.validate_admin_entity
  ;;
end
