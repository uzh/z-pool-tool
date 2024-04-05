open CCFun.Infix
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.settings_role_permission"
let active_navigation = "/admin/settings/role-permission"
let error_path = "/admin/dashboard"
let create_layout = General.create_tenant_layout
let role_permission_path = HttpUtils.Url.Admin.role_permission_path

let role_from_request req =
  let open Role.Role in
  let open CCResult.Infix in
  HttpUtils.find_id of_name Field.Role req
  >>= fun role ->
  match CCList.mem ~eq:equal role customizable with
  | false -> Error Pool_common.Message.(Invalid Field.Role)
  | true -> Ok role
;;

let rule_from_request req role =
  let open Guard in
  let read str =
    CCResult.map_err
      Pool_common.Message.authorization
      (try (Yojson.Safe.from_string %> RolePermission.of_yojson) str with
       | _ -> Error "Undefined Yojson for rule.")
  in
  Sihl.Web.Request.to_urlencoded req
  ||> HttpUtils.find_in_urlencoded Field.Rule
  >== read
  >== fun rule ->
  if Role.Role.equal rule.RolePermission.role role
  then Ok rule
  else Error Pool_common.Message.AccessDenied
;;

let index req =
  let result context =
    Lwt_result.map_error (fun err -> err, error_path)
    @@ (Page.Admin.Settings.RolePermission.index context
        |> create_layout req context
        >|+ Sihl.Web.Response.of_html)
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req =
  HttpUtils.Htmx.handler
    ~active_navigation
    ~error_path:"/"
    ~query:(module Guard.RolePermission)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun ({ Pool_context.database_label; user; _ } as context) query ->
  let* role = role_from_request req |> Lwt_result.lift in
  let%lwt actor =
    Pool_context.Utils.find_authorizable_opt database_label user
  in
  (* TODO: check only available permissions *)
  let%lwt permissions, query =
    match actor with
    | None -> Lwt.return ([], query)
    | Some _actor ->
      Guard.Persistence.RolePermission.query_by_role database_label query role
  in
  let open Page.Admin.Settings.RolePermission in
  (if HttpUtils.Htmx.is_hx_request req then list else show)
    context
    role
    permissions
    query
  |> Lwt_result.return
;;

let delete req =
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err -> err, error_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* role = role_from_request req |> Lwt_result.lift in
    let redirect actions =
      let path = HttpUtils.Url.Admin.role_permission_path ~role () in
      Http_utils.redirect_to_with_actions path actions
    in
    let* rule = rule_from_request req role in
    let%lwt events =
      Cqrs_command.Guardian_command.DeleteRolePermission.handle ~tags rule
      |> Lwt_result.lift
    in
    let handle =
      let open Pool_common.Message in
      function
      | Ok events ->
        let%lwt () =
          Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
        in
        redirect [ Message.set ~success:[ Deleted Field.Rule ] ]
      | Error _ -> redirect [ Message.set ~error:[ NotFound Field.Rule ] ]
    in
    events |> handle |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access

  let read =
    Guard.Access.Permission.read |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete =
    Cqrs_command.Guardian_command.DeleteRolePermission.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
