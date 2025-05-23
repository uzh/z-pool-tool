open CCFun.Infix
open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.settings_role_permission"
let active_navigation = "/admin/settings/role-permission"
let error_path = "/admin/dashboard"
let create_layout = General.create_tenant_layout
let role_permission_path = HttpUtils.Url.Admin.role_permission_path

let group_by_target query permissions =
  let open Guard in
  let open Hashtbl in
  let targets =
    let open Query in
    let open Sort.SortOrder in
    let all = Role.Target.customizable in
    let sort =
      let default = Ascending in
      query.sort
      |> CCOption.map (fun { Sort.column; order } ->
        if Column.equal column RolePermission.column_target then order else default)
      |> CCOption.value ~default
    in
    match sort with
    | Ascending -> all
    | Descending -> CCList.rev all
  in
  let tbl : (Role.Target.t, Permission.t list) t = create (CCList.length targets) in
  let find = find_opt tbl in
  let add = replace tbl in
  let () =
    CCList.iter
      (fun { RolePermission.permission; model; _ } ->
         find model
         |> function
         | None -> add model [ permission ]
         | Some permissions -> add model (permission :: permissions))
      permissions
  in
  targets
  |> CCList.map (fun target ->
    let permissions = find_opt tbl target |> CCOption.value ~default:[] in
    target, permissions)
;;

let role_from_request req =
  let open Role.Role in
  let open CCResult.Infix in
  HttpUtils.find_id of_name Field.Role req
  >>= fun role ->
  match CCList.mem ~eq:equal role customizable with
  | false -> Error (Error.Invalid Field.Role)
  | true -> Ok role
;;

let target_from_request req =
  let open CCResult.Infix in
  let open Role.Target in
  HttpUtils.find_id of_name Field.Target req
  >>= fun target ->
  match CCList.mem ~eq:equal target customizable with
  | false -> Error (Error.Invalid Field.Target)
  | true -> Ok target
;;

let rule_from_request req role =
  let open Guard in
  let read str =
    CCResult.map_err
      Error.authorization
      (try (Yojson.Safe.from_string %> RolePermission.of_yojson) str with
       | _ -> Error "Undefined Yojson for rule.")
  in
  Sihl.Web.Request.to_urlencoded req
  ||> HttpUtils.find_in_urlencoded Field.Rule
  >== read
  >== fun rule ->
  if Role.Role.equal rule.RolePermission.role role
  then Ok rule
  else Error Error.AccessDenied
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let* actor_id =
      user
      |> Pool_context.get_admin_user
      |> Lwt_result.lift
      >|+ Admin.id
      >|+ Admin.Id.to_common
      >|- CCFun.const Response.access_denied
    in
    Response.bad_request_render_error context
    @@
    let* roles =
      Guard.Persistence.Role.find_by_actor_and_permission
        database_label
        actor_id
        Guard.Permission.[ Read ]
    in
    Page.Admin.Settings.RolePermission.index context roles
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let show req =
  Response.Htmx.index_handler
    ~active_navigation
    ~query:(module Guard.RolePermission)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let* role = role_from_request req |> Lwt_result.lift in
  (* TODO: check only available permissions *)
  let%lwt permissions =
    Guard.Persistence.RolePermission.query_by_role database_label role
    ||> fst
    ||> group_by_target query
  in
  let open Page.Admin.Settings.RolePermission in
  (if HttpUtils.Htmx.is_hx_request req then list else show) context role permissions query
  |> Lwt_result.return
;;

let edit_htmx req =
  let result ({ Pool_context.database_label; _ } as context) =
    let* role = role_from_request req |> Lwt_result.lift in
    let* target = target_from_request req |> Lwt_result.lift in
    let%lwt permissions =
      Guard.Persistence.RolePermission.permissions_by_role_and_target
        database_label
        role
        target
    in
    Page.Admin.Settings.RolePermission.edit_target_modal context role target permissions
    |> Response.Htmx.of_html
    |> Lwt.return_ok
  in
  Response.Htmx.handle ~error_as_notification:true ~src req result
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let open Cqrs_command.Guardian_command in
    let open Guard in
    let tags = Pool_context.Logger.Tags.req req in
    let* role = role_from_request req |> Lwt_result.lift in
    let* target = target_from_request req |> Lwt_result.lift in
    let%lwt current_permissions =
      Persistence.RolePermission.permissions_by_role_and_target database_label role target
    in
    let events =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values Permission.(all |> CCList.map show)
      ||> UpdateRolePermissions.decode
      >== UpdateRolePermissions.handle ~tags role target current_permissions
    in
    let handle =
      let open HttpUtils in
      function
      | Ok events ->
        let%lwt () = Pool_event.handle_events ~tags database_label user events in
        Response.Htmx.redirect
          ~actions:[ Message.set ~success:[ Success.Updated Field.Permission ] ]
          (Url.Admin.role_permission_path ~role ())
        |> Lwt_result.ok
      | Error error ->
        Page.Admin.Settings.RolePermission.edit_target_modal
          ~error
          context
          role
          target
          current_permissions
        |> Response.Htmx.of_html
        |> Lwt_result.return
    in
    events >|> handle
  in
  Response.Htmx.handle ~error_as_notification:true ~src req result
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access

  let validate = Middleware.Guardian.validate_admin_entity
  let read = Guard.Access.Permission.read |> validate
  let delete = Cqrs_command.Guardian_command.DeleteRolePermission.effects |> validate
end
