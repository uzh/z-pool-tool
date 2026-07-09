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
  ||> Webserver.Response.of_html
;;

let create req =
  let open Database.Pool in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt multipart_encoded =
    Webserver.Request.to_multipart_form_data_exn req
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
    ||> Webserver.Response.of_html
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let create_operator req =
  let tenant_id =
    HttpUtils.get_field_router_param req Field.Tenant |> Pool_tenant.Id.of_string
  in
  let redirect_path = pool_path ~id:tenant_id ~suffix:"operator" () in
  let%lwt urlencoded = Webserver.Request.to_urlencoded req in
  let result ({ Pool_context.language; user; _ } as context) =
    let tags = Pool_context.Logger.Tags.req req in
    let* tenant = Pool_tenant.find tenant_id in
    let tenant_db = tenant.Pool_tenant.database_label in
    let* email =
      HttpUtils.find_in_urlencoded
        ~error:Error.EmailAddressMissingAdmin
        Field.Email
        urlencoded
      |> Lwt_result.lift
      >== Pool_user.EmailAddress.create
    in
    let%lwt existing_user = Pool_user.find_by_email_opt tenant_db email in
    match existing_user with
    | Some existing when Pool_user.is_admin existing ->
      let* admin = Admin.find tenant_db (Admin.Id.of_user existing.Pool_user.id) in
      Page.Root.Tenant.operator_existing_admin_modal context tenant_id admin
      |> Response.Htmx.of_html
      |> Lwt_result.return
    | Some existing ->
      let* contact = Contact.find tenant_db (Contact.Id.of_user existing.Pool_user.id) in
      Page.Root.Tenant.operator_existing_contact_modal context tenant contact
      |> Response.Htmx.of_html
      |> Lwt_result.return
    | None ->
      let id = Admin.Id.create () in
      let* admin_events =
        let open Cqrs_command.Admin_command.CreateOperator in
        let* cmd = decode urlencoded |> Lwt_result.lift in
        let password =
          (* Random dummy password; the operator sets their own via the password
             reset link sent below. *)
          Format.asprintf "%s-N1!" (Pool_core.Random.base64 32)
          |> Pool_user.Password.Plain.create
        in
        handle ~id ~tags ~password cmd |> Lwt_result.lift
      in
      let%lwt () = Pool_event.handle_events ~tags tenant_db user admin_events in
      let* reset_email =
        let* admin_user =
          Pool_user.find_by_email_opt tenant_db email
          ||> CCOption.to_result (Error.NotFound Field.Admin)
        in
        Message_template.PasswordReset.create
          tenant_db
          language
          (Message_template.Tenant tenant)
          admin_user
      in
      (* Dispatch the email on the root queue so it is delivered through the
         root SMTP configuration; the tenant SMTP may not be set up yet. *)
      let%lwt () =
        Pool_event.handle_event
          ~tags
          Database.Pool.Root.label
          user
          (Email.sent reset_email |> Pool_event.email)
      in
      Response.Htmx.redirect
        redirect_path
        ~actions:[ Message.set ~success:[ Success.Created Field.Operator ] ]
      |> Lwt_result.ok
  in
  (* On error, keep the modal placeholder alive (the form targets it) and show
     the error as a notification. *)
  let response ({ Pool_context.language; _ } as context) =
    let tags = Pool_context.Logger.Tags.req req in
    result context
    ||> CCResult.get_lazy (fun err ->
      let err = Pool_common.Utils.with_log_error ~src ~tags err in
      Response.Htmx.of_html_list
        [ Response.Htmx.error_notification language err
        ; Component.Modal.create_placeholder Page.Root.Tenant.operator_modal_id
        ])
    ||> CCResult.return
  in
  Response.Htmx.handle ~src req response
;;

let promote_operator req =
  let tenant_id =
    HttpUtils.get_field_router_param req Field.Tenant |> Pool_tenant.Id.of_string
  in
  let redirect_path = pool_path ~id:tenant_id ~suffix:"operator" () in
  let%lwt urlencoded = Webserver.Request.to_urlencoded req in
  let result { Pool_context.user; _ } =
    Response.bad_request_on_error ~urlencoded manage_operators
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* tenant = Pool_tenant.find tenant_id in
    let tenant_db = tenant.Pool_tenant.database_label in
    let* admin =
      HttpUtils.find_in_urlencoded Field.Admin urlencoded
      |> Lwt_result.lift
      >>= fun id -> Admin.find tenant_db (Admin.Id.of_string id)
    in
    let* events =
      let open Cqrs_command.Guardian_command in
      let target_id = Admin.id admin |> Guard.Uuid.actor_of Admin.Id.value in
      GrantRoles.handle ~tags { target_id; roles = [ `Operator, None ] }
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags tenant_db user events in
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set ~success:[ Success.RoleAssigned ] ]
    |> Lwt_result.ok
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
    ||> Webserver.Response.of_html
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
