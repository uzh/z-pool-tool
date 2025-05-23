open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Database = Database
module Response = Http_response

let src = Logs.Src.create "handler.root.tenant"
let pool_path = Http_utils.Url.Root.pool_path
let active_navigation = pool_path ()

let tenants req =
  let context = Pool_context.find_exn req in
  let%lwt tenant_list = Pool_tenant.find_all () in
  Page.Root.Tenant.list tenant_list context
  |> General.create_root_layout ~active_navigation context
  ||> Sihl.Web.Response.of_html
;;

let create req =
  let open Database.Pool in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt multipart_encoded =
    Sihl.Web.Request.to_multipart_form_data_exn req
    ||> HttpUtils.remove_empty_values_multiplart
  in
  let urlencoded =
    multipart_encoded |> HttpUtils.multipart_to_urlencoded Pool_tenant.file_fields
  in
  let result { Pool_context.user; _ } =
    Response.bad_request_on_error ~urlencoded tenants
    @@
    let events () =
      let open Cqrs_command.Pool_tenant_command in
      let* database =
        let* { database_url; database_label } =
          decode_database urlencoded |> Lwt_result.lift
        in
        create_tested database_label database_url
      in
      let* files =
        HttpUtils.File.upload_files
          Root.label
          (CCList.map Field.show Pool_tenant.file_fields)
          req
      in
      let* (decoded : create) =
        files @ multipart_encoded
        |> HttpUtils.File.multipart_form_data_to_urlencoded
        |> Create.decode
        |> Lwt_result.lift
      in
      let events = Create.handle ~tags database decoded |> Lwt_result.lift in
      events >|> HttpUtils.File.cleanup_upload Root.label files
    in
    let handle = Pool_event.handle_events Database.Pool.Root.label user in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        (pool_path ())
        [ Message.set ~success:[ Success.Created Field.Tenant ] ]
    in
    () |> events |>> handle |>> return_to_overview
  in
  Response.handle ~src req result
;;

let manage_operators req =
  let result context =
    Response.bad_request_render_error context
    @@
    let id =
      HttpUtils.get_field_router_param req Field.tenant |> Pool_tenant.Id.of_string
    in
    let* tenant = Pool_tenant.find id in
    let%lwt operators =
      Admin.find_all_with_role tenant.Pool_tenant.database_label (`Operator, None)
    in
    Page.Root.Tenant.manage_operators tenant operators context
    |> General.create_root_layout context
    ||> Sihl.Web.Response.of_html
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let create_operator req =
  let tenant_id =
    HttpUtils.get_field_router_param req Field.Tenant |> Pool_tenant.Id.of_string
  in
  let redirect_path = pool_path ~id:tenant_id () in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.user; _ } =
    Response.bad_request_on_error ~urlencoded manage_operators
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* tenant_db = Pool_tenant.(find_full tenant_id >|+ Write.database_label) in
    let validate_user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result Error.EmailAddressMissingAdmin
      >== Pool_user.EmailAddress.create
      >>= HttpUtils.validate_email_existance tenant_db
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Admin_command.CreateAdmin in
      urlencoded |> decode >>= handle ~roles:[ `Operator, None ] ~tags |> Lwt_result.lift
    in
    let handle events =
      events |> Pool_event.handle_events ~tags tenant_db user |> Lwt_result.ok
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Success.Created Field.Operator ] ]
    in
    validate_user () >> events >>= handle |>> return_to_overview
  in
  Response.handle ~src req result
;;

let tenant_detail req =
  let result context =
    Response.bad_request_render_error context
    @@
    let id =
      HttpUtils.get_field_router_param req Field.tenant |> Pool_tenant.Id.of_string
    in
    let* tenant = Pool_tenant.find id in
    Page.Root.Tenant.detail tenant context
    |> General.create_root_layout context
    ||> Sihl.Web.Response.of_html
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

module Access : sig
  include module type of Helpers.Access

  val create_operator : Rock.Middleware.t
  val read_operator : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Access = Pool_tenant.Guard.Access
  module Guardian = Middleware.Guardian
  module TenantCommand = Cqrs_command.Pool_tenant_command

  let tenant_effects = Guardian.id_effects Pool_tenant.Id.validate Field.Tenant
  let index = Access.index |> Guardian.validate_admin_entity
  let create = Guardian.validate_admin_entity TenantCommand.Create.effects
  let read = tenant_effects Access.read
  let update = tenant_effects TenantCommand.EditDetails.effects
  let read_operator = Admin.Guard.Access.index |> Guardian.validate_admin_entity

  let create_operator =
    let open Guard in
    let open ValidationSet in
    [ Permission.Manage, `Admin, None; Permission.Manage, `Role, None ]
    |> CCList.map one_of_tuple
    |> and_
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
