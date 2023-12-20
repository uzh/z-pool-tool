open CCFun.Infix
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.settings_role_permission"
let active_navigation = "/admin/settings/role-permission"

let show req =
  HttpUtils.Htmx.handler
    ~active_navigation
    ~error_path:"/"
    ~query:(module Guard)
    ~create_layout:General.create_tenant_layout
    (fun ({ Pool_context.database_label; user; _ } as context) query ->
      let%lwt actor =
        Pool_context.Utils.find_authorizable_opt database_label user
      in
      (* TODO: check only available permissions *)
      let%lwt permissions, query =
        match actor with
        | None -> Lwt.return ([], query)
        | Some _actor ->
          Guard.Persistence.RolePermission.find_by query database_label
      in
      let page =
        let open Page.Admin.Settings.RolePermission in
        (if HttpUtils.Htmx.is_hx_request req then list else index)
          context
          permissions
          query
      in
      Lwt_result.return page)
    req
;;

let delete req =
  let result { Pool_context.database_label; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let rule =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.find_in_urlencoded Field.Rule
      >== fun rule ->
      let read = Yojson.Safe.from_string %> Guard.RolePermission.of_yojson in
      CCResult.map_err
        Pool_common.Message.authorization
        (try read rule with
         | _ -> Error "Undefined Yojson for rule.")
    in
    let events =
      Cqrs_command.Guardian_command.DeleteRolePermission.handle ~tags
    in
    let handle = function
      | Ok events ->
        let%lwt () =
          Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
        in
        Http_utils.redirect_to_with_actions
          active_navigation
          [ Message.set ~success:[ Pool_common.Message.(Deleted Field.Rule) ] ]
      | Error _ ->
        Http_utils.redirect_to_with_actions
          active_navigation
          [ Message.set ~error:[ Pool_common.Message.(NotFound Field.Rule) ] ]
    in
    rule >== events >|> handle |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access

  let index =
    Guard.Access.read_permission |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete =
    Cqrs_command.Guardian_command.DeleteRolePermission.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
