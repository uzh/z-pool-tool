open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.settings_role_assignment"
let active_navigation = "/admin/settings/role-assignment"

let show req =
  let open CCFun in
  HttpUtils.Htmx.handler
    ~active_navigation
    ~error_path:"/"
    ~query:(module Guard.RoleAssignment)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun ({ Pool_context.database_label; user; _ } as context) query ->
  let open Page.Admin.Settings.RoleAssignment in
  Pool_context.Utils.find_authorizable_opt database_label user
  >|> (function
         | None -> Lwt.return ([], query)
         | Some (_ : Guard.Actor.t) ->
           Guard.Persistence.RoleAssignment.find_all ~query database_label)
  ||> uncurry
        ((if HttpUtils.Htmx.is_hx_request req then list else index) context)
  |> Lwt_result.ok
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Page.Admin.Settings.RoleAssignment.create
      context
      (CCFun.flip Sihl.Web.Flash.find req)
    |> General.create_tenant_layout req context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, active_navigation
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let handle_form action req =
  let open CCFun in
  let error = Pool_common.Message.[ NotFound Field.AssignableRole ] in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* admin =
      Pool_context.get_admin_user user
      |> Lwt_result.lift
      >|- fun err -> err, active_navigation
    in
    let events, success =
      let open Pool_common.Message in
      let open Cqrs_command.Guardian_command in
      match action with
      | `Create ->
        CreateRoleAssignment.handle ~tags, [ Created Field.AssignableRole ]
      | `Delete ->
        let comment =
          Format.asprintf "by admin: %s" Admin.([%show: Id.t] (id admin))
        in
        ( DeleteRoleAssignment.handle ~comment ~tags
        , [ Deleted Field.AssignableRole ] )
    in
    let* role_assignment =
      let open CCResult in
      let open Pool_common.Message in
      Utils.Lwt_result.map_error (fun err -> err, active_navigation)
      @@
      let find_in_urlencoded = flip HttpUtils.find_in_urlencoded urlencoded in
      match action with
      | `Create ->
        let read_role field =
          find_in_urlencoded field
          >>= Role.Role.of_string_res %> map_err authorization
        in
        both (read_role Field.Role) (read_role Field.AssignableRole)
        |> Lwt_result.lift
        >|+ uncurry Guard.RoleAssignment.create
      | `Delete ->
        find_in_urlencoded Field.Role
        |> Lwt_result.lift
        >|+ Yojson.Safe.from_string
        >== Guard.RoleAssignment.of_yojson %> map_err authorization
    in
    let handle roles =
      let redirect = HttpUtils.redirect_to_with_actions active_navigation in
      roles
      |> Lwt_result.lift
      |>> Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
      >|> function
      | Ok () -> redirect [ Message.set ~success ]
      | Error _ -> redirect [ Message.set ~error ]
    in
    role_assignment |> events |> handle |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let create = handle_form `Create
let delete = handle_form `Delete

module Access : module type of Helpers.Access = struct
  include Helpers.Access

  let index =
    Guard.Access.RoleAssignment.read
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let create =
    Cqrs_command.Guardian_command.CreateRoleAssignment.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete =
    Cqrs_command.Guardian_command.DeleteRoleAssignment.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
