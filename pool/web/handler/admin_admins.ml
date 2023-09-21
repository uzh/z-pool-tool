open CCFun
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.admins"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req

let index req =
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@
    let%lwt admin_users = Admin.find_all database_label () in
    Page.Admin.Admins.index context admin_users
    |> create_layout req ~active_navigation:"/admin/admins" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let admin_detail req is_edit =
  let result ({ Pool_context.csrf; database_label; language; _ } as context) =
    let%lwt available_roles =
      Helpers.Guard.find_roles_of_ctx context
      ||> CCList.flat_map (fun { Guard.ActorRole.role; _ } ->
        Role.Role.can_assign_roles role)
    in
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@
    let id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
    let* admin = id |> Admin.find database_label in
    let%lwt roles =
      let open Helpers.Guard in
      find_roles database_label admin
      >|> Lwt_list.map_s (target_model_for_actor_role database_label)
    in
    let* () =
      let* _ = General.admin_from_session database_label req in
      Lwt.return_ok ()
    in
    (match is_edit with
     | true ->
       Component.Role.Search.input_form csrf language admin available_roles ()
       |> CCList.return
       |> Page.Admin.Admins.edit context admin roles
     | false -> Page.Admin.Admins.detail context admin roles)
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let detail req = admin_detail req false
let edit req = admin_detail req true

let new_form req =
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@ (Page.Admin.Admins.new_form context
        |> create_layout req context
        >|+ Sihl.Web.Response.of_html)
  in
  result |> extract_happy_path req
;;

let create_admin req =
  let open Pool_common.Message in
  let redirect_path = Format.asprintf "/admin/admins" in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err ->
      err, Format.asprintf "%s/new" redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let id = Admin.Id.create () in
    let validate_user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result EmailAddressMissingAdmin
      >>= HttpUtils.validate_email_existance database_label
    in
    let events =
      let open Cqrs_command.Admin_command.CreateAdmin in
      Sihl.Web.Request.to_urlencoded req ||> decode >== handle ~id ~tags
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      |> Lwt_result.ok
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        (Format.asprintf "%s/%s" redirect_path (Admin.Id.value id))
        [ Message.set ~success:[ Created Field.Admin ] ]
    in
    () |> validate_user >> events >>= handle |>> return_to_overview
  in
  result |> extract_happy_path req
;;

let handle_toggle_role req =
  let result (_ : Pool_context.t) =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.find_in_urlencoded Field.Role
    >== Role.Role.of_string_res
        %> CCResult.map_err Pool_common.Message.authorization
    >|+ fun key ->
    let exclude_roles_of =
      try
        HttpUtils.find_id Admin.Id.of_string Field.Admin req |> CCOption.return
      with
      | _ -> None
    in
    Component.Role.Search.value_form
      Pool_common.Language.En
      ?exclude_roles_of
      ~key
      ()
    |> HttpUtils.Htmx.html_to_plain_text_response
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let grant_role ({ Rock.Request.target; _ } as req) =
  let open Utils.Lwt_result.Infix in
  let lift = Lwt_result.lift in
  let result { Pool_context.database_label; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let* admin =
      HttpUtils.find_id Admin.Id.of_string Field.Admin req
      |> Admin.find database_label
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* role =
      HttpUtils.find_in_urlencoded Field.Role urlencoded
      |> lift
      >== Role.Role.of_string_res
          %> CCResult.map_err Pool_common.Message.authorization
    in
    let* role_target =
      HttpUtils.htmx_urlencoded_list Field.(Target |> array_key) req
      ||> CCList.map
            (Guard.Uuid.Target.of_string
             %> CCOption.to_result Pool_common.Message.(Decode Field.Id))
      ||> CCResult.flatten_l
    in
    let expand_targets =
      let open Guard.Uuid.Target in
      if role_target |> CCList.is_empty
      then Lwt.return_ok [ role, None ]
      else (
        match role with
        | `Assistant | `Experimenter ->
          let to_id = to_string %> Experiment.Id.of_string in
          role_target
          |> Lwt_list.filter_s (fun id ->
            id |> to_id |> Experiment.find database_label ||> CCResult.is_ok)
          ||> CCList.map (fun uuid -> role, Some uuid)
          |> Lwt_result.ok
        | `LocationManager ->
          let to_id = to_string %> Pool_location.Id.of_string in
          role_target
          |> Lwt_list.filter_s (fun id ->
            id |> to_id |> Pool_location.find database_label ||> CCResult.is_ok)
          ||> CCList.map (fun uuid -> role, Some uuid)
          |> Lwt_result.ok
        | role ->
          Logs.err (fun m ->
            m "Admin handler: Missing role %s" ([%show: Role.Role.t] role));
          Lwt.return_error Pool_common.Message.(NotFound Field.Role)
          ||> Pool_common.Utils.with_log_result_error ~src ~tags CCFun.id)
    in
    let events roles =
      let open Cqrs_command.Guardian_command in
      (* TODO: validate if role can be granted *)
      GrantRoles.handle ~tags { target = admin; roles } |> lift
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
    in
    expand_targets
    >>= events
    |>> handle
    |>> HttpUtils.Htmx.htmx_redirect
          ~skip_externalize:true
          (CCString.replace ~which:`Right ~sub:"/grant-role" ~by:"/edit" target)
          ~actions:
            [ Message.set ~success:[ Pool_common.Message.Created Field.Role ] ]
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let revoke_role ({ Rock.Request.target; _ } as req) =
  let open Utils.Lwt_result.Infix in
  let edit_route =
    CCString.replace ~which:`Right ~sub:"/revoke-role" ~by:"/edit" target
  in
  let result { Pool_context.database_label; _ } =
    (let tags = Pool_context.Logger.Tags.req req in
     let* admin =
       HttpUtils.find_id Admin.Id.of_string Field.Admin req
       |> Admin.find database_label
     in
     let role =
       let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
       let role =
         let open CCResult in
         HttpUtils.find_in_urlencoded Field.Role urlencoded
         >>= Role.Role.of_string_res
             %> CCResult.map_err Pool_common.Message.authorization
       in
       let uuid =
         HttpUtils.find_in_urlencoded_opt Field.Target urlencoded
         |> flip CCOption.bind Guard.Uuid.Target.of_string
       in
       role |> Lwt_result.lift >|+ fun role -> role, uuid
     in
     let events role =
       let open Cqrs_command.Guardian_command in
       RevokeRole.handle ~tags { target = admin; role } |> Lwt_result.lift
     in
     let handle events =
       let%lwt () =
         Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
       in
       Http_utils.redirect_to_with_actions
         edit_route
         [ Message.set ~success:[ Pool_common.Message.RoleUnassigned ] ]
     in
     role >>= events |>> handle)
    >|- fun err -> err, edit_route
  in
  result |> extract_happy_path req
;;

module Access : sig
  include module type of Helpers.Access

  val grant_role : Rock.Middleware.t
  val revoke_role : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Command = Cqrs_command.Admin_command
  module Guardian = Middleware.Guardian
  module GuardianCommand = Cqrs_command.Guardian_command

  let admin_effects = Guardian.id_effects Admin.Id.of_string Field.Admin

  let index =
    Admin.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create = Command.CreateAdmin.effects |> Guardian.validate_admin_entity

  let read =
    Admin.Guard.Access.read |> admin_effects |> Guardian.validate_generic
  ;;

  let update =
    Admin.Guard.Access.update
    |> admin_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let grant_role =
    GuardianCommand.GrantRoles.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let revoke_role =
    GuardianCommand.RevokeRole.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
