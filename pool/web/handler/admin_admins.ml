module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@
    let%lwt admin_users = Admin.find_all database_label () in
    Page.Admin.Admins.index context admin_users
    |> create_layout req ~active_navigation:"/admin/admins" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let admin_detail req is_edit =
  (* TODO: Impelement authorization *)
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@
    let id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
    let* admin = id |> Admin.find database_label in
    let* () =
      let* _ = General.admin_from_session database_label req in
      Lwt.return_ok ()
    in
    (match is_edit with
     | true -> Page.Admin.Admins.edit context admin
     | false -> Page.Admin.Admins.detail context admin)
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail req = admin_detail req false
let edit req = admin_detail req true

let new_form req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@ (Page.Admin.Admins.new_form context
        |> create_layout req context
        >|+ Sihl.Web.Response.of_html)
  in
  result |> HttpUtils.extract_happy_path req
;;

let create_admin req =
  let open Pool_common.Message in
  let open Utils.Lwt_result.Infix in
  let redirect_path = Format.asprintf "/admin/admins" in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err ->
      err, Format.asprintf "%s/new" redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let admin_id = Pool_common.Id.create () in
    let validate_user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result EmailAddressMissingAdmin
      >>= HttpUtils.validate_email_existance database_label
    in
    let events =
      let open Cqrs_command.Admin_command.CreateAdmin in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      CCResult.(urlencoded |> decode >>= handle ~id:admin_id ~tags)
      |> Lwt_result.lift
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      |> Lwt_result.ok
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        (Format.asprintf "%s/%s" redirect_path (Pool_common.Id.value admin_id))
        [ Message.set ~success:[ Created Field.Admin ] ]
    in
    () |> validate_user >> events >>= handle |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  open Guard

  let admin_effects =
    Middleware.Guardian.id_effects Admin.Id.of_string Field.Admin
  ;;

  let create =
    Cqrs_command.Admin_command.CreateAdmin.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let read =
    (fun id ->
      let target_id = id |> Uuid.target_of Admin.Id.value in
      ValidationSet.(
        Or
          [ One (Action.Read, TargetSpec.Entity `Admin)
          ; One (Action.Read, TargetSpec.Id (`Admin, target_id))
          ]))
    |> admin_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    (fun id ->
      let target_id = id |> Uuid.target_of Admin.Id.value in
      ValidationSet.(
        Or
          [ One (Action.Update, TargetSpec.Entity `Admin)
          ; One (Action.Update, TargetSpec.Id (`Admin, target_id))
          ]))
    |> admin_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let index =
    ValidationSet.One (Action.Read, TargetSpec.Entity `Admin)
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
