open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.admins"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req

let find_authorizable_target database_label req =
  let open Utils.Lwt_result.Infix in
  HttpUtils.find_id Admin.Id.of_string Field.Admin req
  |> Guard.Uuid.target_of Admin.Id.value
  |> Guard.Persistence.Target.find ~ctx:(Database.to_ctx database_label)
  >|- CCFun.const (Pool_message.Error.NotFound Field.Target)
;;

let index req =
  HttpUtils.Htmx.handler
    ~active_navigation:"/admin/admins"
    ~error_path:"/admin/dashboard"
    ~query:(module Admin)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun (Pool_context.{ database_label; user; _ } as context) query ->
  let* actor =
    Pool_context.Utils.find_authorizable ~admin_only:true database_label user
  in
  let%lwt admins = Admin.list_by_user ~query database_label actor in
  let open Page.Admin.Admins in
  Lwt_result.return
  @@
  if HttpUtils.Htmx.is_hx_request req then list context admins else index context admins
;;

let admin_detail req is_edit =
  let result ({ Pool_context.csrf; database_label; language; user; _ } as context) =
    let%lwt actor =
      Pool_context.Utils.find_authorizable_opt ~admin_only:true database_label user
    in
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@
    let id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
    let* admin = id |> Admin.find database_label in
    let target_id = Guard.Uuid.target_of Admin.Id.value (Admin.id admin) in
    let%lwt roles =
      Pool_context.Admin admin
      |> Pool_context.Utils.find_authorizable_opt database_label
      >|> Helpers.Guard.find_roles database_label
    in
    let* () =
      let* _ = General.admin_from_session database_label req in
      Lwt.return_ok ()
    in
    (match is_edit with
     | true ->
       let%lwt available_roles =
         CCOption.map_or
           ~default:(Lwt.return [])
           (Guard.Persistence.Actor.can_assign_roles database_label)
           actor
         ||> CCList.map fst
       in
       Component.Role.Search.input_form
         ~path:"/admin/admins"
         csrf
         language
         target_id
         available_roles
         ()
       |> CCList.return
       |> Page.Admin.Admins.edit context admin target_id roles
       |> Lwt.return
     | false -> Page.Admin.Admins.detail context admin target_id roles |> Lwt.return)
    >|> create_layout req context
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
  let redirect_path = Format.asprintf "/admin/admins" in
  let result { Pool_context.database_label; user; _ } =
    Lwt_result.map_error (fun err -> err, Format.asprintf "%s/new" redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let id = Admin.Id.create () in
    let validate_user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result Error.EmailAddressMissingAdmin
      >== Pool_user.EmailAddress.create
      >>= HttpUtils.validate_email_existance database_label
    in
    let events =
      let open Cqrs_command.Admin_command.CreateAdmin in
      Sihl.Web.Request.to_urlencoded req ||> decode >== handle ~id ~tags
    in
    let handle events =
      Pool_event.handle_events ~tags database_label user events |> Lwt_result.ok
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        (Format.asprintf "%s/%s" redirect_path (Admin.Id.value id))
        [ Message.set ~success:[ Success.Created Field.Admin ] ]
    in
    () |> validate_user >> events >>= handle |>> return_to_overview
  in
  result |> extract_happy_path req
;;

let handle_toggle_role req =
  let result (_ : Pool_context.t) =
    let target_id =
      HttpUtils.find_id Admin.Id.of_string Field.Admin req
      |> Guard.Uuid.target_of Admin.Id.value
    in
    Helpers.Guard.handle_toggle_role target_id req |> Lwt_result.ok
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let search_role_entities req =
  let result { Pool_context.database_label; _ } =
    let* target = find_authorizable_target database_label req in
    Helpers.Guard.search_role_entities target req |> Lwt_result.ok
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let grant_role req =
  let open Utils.Lwt_result.Infix in
  let admin_id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
  let to_guardian_id admin = admin |> Admin.id |> Guard.Uuid.actor_of Admin.Id.value in
  let redirect_path = Format.asprintf "/admin/admins/%s/edit" (Admin.Id.value admin_id) in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let* admin = Admin.find database_label admin_id in
    let target_id = to_guardian_id admin in
    Helpers.Guard.grant_role ~redirect_path ~user ~target_id database_label req
  in
  result |> extract_happy_path req
;;

let revoke_role ({ Rock.Request.target; _ } as req) =
  let open Utils.Lwt_result.Infix in
  let redirect_path =
    CCString.replace ~which:`Right ~sub:"/revoke-role" ~by:"/edit" target
  in
  let result { Pool_context.database_label; user; _ } =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let* target_id =
      HttpUtils.find_id Admin.Id.of_string Field.Admin req
      |> Admin.find database_label
      >|+ Admin.id
      >|+ Guard.Uuid.actor_of Admin.Id.value
    in
    Helpers.Guard.revoke_role ~redirect_path ~user ~target_id database_label req
  in
  result |> extract_happy_path req
;;

let search = Helpers.Search.htmx_search_helper ~query_field:Field.(SearchOf Admin) `Admin

module Access : sig
  include module type of Helpers.Access

  val grant_role : Rock.Middleware.t
  val revoke_role : Rock.Middleware.t
  val search : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Command = Cqrs_command.Admin_command
  module GuardianCommand = Cqrs_command.Guardian_command
  module Guardian = Middleware.Guardian

  let admin_effects = Guardian.id_effects Admin.Id.validate Field.Admin
  let index = Admin.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  let create = Command.CreateAdmin.effects |> Guardian.validate_admin_entity
  let read = admin_effects Admin.Guard.Access.read
  let update = admin_effects Admin.Guard.Access.update

  let grant_role =
    GuardianCommand.GrantRoles.effects |> Middleware.Guardian.validate_admin_entity
  ;;

  let revoke_role =
    GuardianCommand.RevokeRole.effects |> Middleware.Guardian.validate_admin_entity
  ;;

  let search = index
end
