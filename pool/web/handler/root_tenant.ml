module Common = Pool_common
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Update = Root_tenant_update
module Database = Pool_database

let tenants req =
  let context = Pool_context.find_exn req in
  let%lwt tenant_list = Pool_tenant.find_all () in
  let%lwt root_list = Root.find_all () in
  Page.Root.Tenant.list tenant_list root_list context
  |> General.create_root_layout ~active_navigation:"/root/tenants" context
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.tenant_db; _ } =
    let events () =
      Lwt_result.map_error (fun err -> err, "/root/tenants")
      @@
      let open Lwt_result.Syntax in
      let%lwt multipart_encoded =
        Sihl.Web.Request.to_multipart_form_data_exn req
      in
      let file_fields =
        let open Pool_common.Message.Field in
        [ Styles; Icon ] @ Pool_tenant.LogoMapping.LogoType.all_fields
        |> CCList.map show
      in
      let* files = File.upload_files Database.root file_fields req in
      let finalize = function
        | Ok resp -> Lwt.return_ok resp
        | Error err ->
          let ctx = tenant_db |> Pool_tenant.to_ctx in
          let%lwt () =
            Lwt_list.iter_s
              (fun (_, id) -> Service.Storage.delete ~ctx id)
              files
          in
          Lwt.return_error err
      in
      let events =
        let open CCResult.Infix in
        let open Cqrs_command.Pool_tenant_command.Create in
        files @ multipart_encoded
        |> File.multipart_form_data_to_urlencoded
        |> decode
        >>= handle
        |> Lwt_result.lift
      in
      events >|> finalize
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        "/root/tenants"
        [ Message.set ~success:[ Common.Message.(Created Field.Tenant) ] ]
    in
    () |> events |>> handle |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let manage_operators req =
  let open Lwt_result.Syntax in
  let open Sihl.Web in
  let result context =
    Lwt_result.map_error (fun err -> err, "/root/tenants")
    @@
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Tenant
      |> Pool_common.Id.of_string
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
  let result { Pool_context.tenant_db; _ } =
    let open Utils.Lwt_result.Infix in
    let open Common.Message in
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Tenant
      |> Pool_common.Id.of_string
    in
    let redirect_path =
      Format.asprintf "/root/tenants/%s" (Pool_common.Id.value id)
    in
    let user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result EmailAddressMissingOperator
      >>= HttpUtils.validate_email_existance tenant_db
    in
    let find_tenant () = Pool_tenant.find_full id in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Admin_command.CreateOperator in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded |> decode >>= handle |> Lwt_result.lift
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
      |> Lwt_result.ok
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Created Field.Operator ] ]
    in
    ()
    |> user
    >>= find_tenant
    >> events
    >>= handle
    |> Lwt_result.map_error (fun err ->
         err, Format.asprintf "%s/operator" redirect_path)
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let tenant_detail req =
  let open Lwt_result.Syntax in
  let open Sihl.Web in
  let result context =
    Lwt_result.map_error (fun err -> err, "/root/tenants")
    @@
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Tenant
      |> Pool_common.Id.of_string
    in
    let* tenant = Pool_tenant.find id in
    Page.Root.Tenant.detail tenant context
    |> General.create_root_layout context
    |> Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path req
;;
