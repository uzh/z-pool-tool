open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field
module File = HttpUtils.File
module Update = Root_tenant_update
module Database = Pool_database

let tenants_path = "/root/tenants"
let active_navigation = tenants_path

let tenants req =
  let context = Pool_context.find_exn req in
  let%lwt tenant_list = Pool_tenant.find_all () in
  let flash_fetcher key = Sihl.Web.Flash.find key req in
  Page.Root.Tenant.list tenant_list context flash_fetcher
  |> General.create_root_layout ~active_navigation context
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;

let create req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt multipart_encoded =
    Sihl.Web.Request.to_multipart_form_data_exn req
    ||> HttpUtils.remove_empty_values_multiplart
  in
  let result _ =
    Utils.Lwt_result.map_error (fun err ->
      let urlencoded =
        multipart_encoded
        |> HttpUtils.multipart_to_urlencoded Pool_tenant.file_fields
      in
      err, tenants_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let events () =
      let open CCFun in
      let* files =
        File.upload_files
          Database.root
          (CCList.map Pool_common.Message.Field.show Pool_tenant.file_fields)
          req
      in
      let events =
        let open CCResult.Infix in
        let open Cqrs_command.Pool_tenant_command.Create in
        files @ multipart_encoded
        |> File.multipart_form_data_to_urlencoded
        |> decode
        >>= handle ~tags
        |> Lwt_result.lift
      in
      events >|> HttpUtils.File.cleanup_upload Database.root files
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags Database.root)
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        tenants_path
        [ Message.set ~success:[ Pool_common.Message.(Created Field.Tenant) ] ]
    in
    () |> events |>> handle |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let manage_operators req =
  let open Sihl.Web in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, tenants_path)
    @@
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Tenant
      |> Pool_tenant.Id.of_string
    in
    let* tenant = Pool_tenant.find id in
    Page.Root.Tenant.manage_operators tenant context
    |> General.create_root_layout context
    |> Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let create_operator req =
  let tenant_id =
    HttpUtils.get_field_router_param req Field.Tenant
    |> Pool_tenant.Id.of_string
  in
  let redirect_path =
    Format.asprintf "/root/tenants/%s" (Pool_tenant.Id.value tenant_id)
  in
  let result _ =
    Lwt_result.map_error (fun err ->
      err, Format.asprintf "%s/operator" redirect_path)
    @@
    let open CCFun in
    let tags = Pool_context.Logger.Tags.req req in
    let* tenant_db =
      Pool_tenant.find_full tenant_id
      >|+ fun { Pool_tenant.Write.database; _ } -> database.Database.label
    in
    let validate_user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result Pool_common.Message.EmailAddressMissingAdmin
      >>= HttpUtils.validate_email_existance tenant_db
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Admin_command.CreateAdmin in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> decode
      >>= handle ~roles:(Guard.ActorRoleSet.singleton `OperatorAll) ~tags
      |> Lwt_result.lift
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags tenant_db) %> Lwt_result.ok
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.Created Field.Operator ] ]
    in
    validate_user () >> events >>= handle |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let tenant_detail req =
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, tenants_path)
    @@
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Tenant
      |> Pool_tenant.Id.of_string
    in
    let* tenant = Pool_tenant.find id in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Root.Tenant.detail tenant context flash_fetcher
    |> General.create_root_layout context
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path req
;;

module Access : sig
  include Helpers.AccessSig

  val create_operator : Rock.Middleware.t
  val read_operator : Rock.Middleware.t
end = struct
  module Field = Pool_common.Message.Field
  module TenantCommand = Cqrs_command.Pool_tenant_command

  let tenant_effects =
    Middleware.Guardian.id_effects Pool_location.Id.of_string Field.Tenant
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity [ `Read, `TargetEntity `Location ]
  ;;

  let create =
    Middleware.Guardian.validate_admin_entity TenantCommand.Create.effects
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Pool_location.Id.value)
        ; `Read, `TargetEntity `Tenant
        ])
    ]
    |> tenant_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    Middleware.Guardian.validate_admin_entity [ `Update, `TargetEntity `Tenant ]
  ;;

  let read_operator =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity (`Admin `Operator) ]
  ;;

  let create_operator =
    Middleware.Guardian.validate_admin_entity
      [ `Create, `TargetEntity (`Admin `Operator) ]
  ;;

  let delete = Middleware.Guardian.denied
end
