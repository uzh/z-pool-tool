module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Common = Pool_common
module Database = Pool_database

let update req command success_message =
  let result _ =
    let open Utils.Lwt_result.Infix in
    let open Common.Message.Field in
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Tenant
      |> Pool_tenant.Id.of_string
    in
    let redirect_path =
      Format.asprintf "/root/tenants/%s" (Pool_tenant.Id.value id)
    in
    let events tenant =
      let open Utils.Lwt_result.Infix in
      let%lwt multipart_encoded =
        Sihl.Web.Request.to_multipart_form_data_exn req
      in
      let* _ =
        File.update_files
          Database.root
          [ ( Styles |> show
            , tenant.Pool_tenant.Write.styles |> Pool_tenant.Styles.Write.value
            )
          ; ( Icon |> show
            , tenant.Pool_tenant.Write.icon |> Pool_tenant.Icon.Write.value )
          ]
          req
      in
      let* logo_files =
        File.upload_files
          Database.root
          (Pool_tenant.LogoMapping.LogoType.all_fields |> CCList.map show)
          req
      in
      let events_list urlencoded =
        let tags = Logger.req req in
        let open CCResult.Infix in
        match command with
        | `EditDetail ->
          Cqrs_command.Pool_tenant_command.EditDetails.(
            decode urlencoded >>= handle ~tags tenant)
        | `EditDatabase ->
          Cqrs_command.Pool_tenant_command.EditDatabase.(
            decode urlencoded >>= handle ~tags tenant)
      in
      logo_files @ multipart_encoded
      |> File.multipart_form_data_to_urlencoded
      |> HttpUtils.format_request_boolean_values [ TenantDisabledFlag |> show ]
      |> events_list
      |> Lwt_result.lift
    in
    let tags = Logger.req req in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags Database.root)
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success_message ] ]
    in
    id
    |> Pool_tenant.find_full
    >>= events
    >|- (fun err -> err, redirect_path)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let update_detail req =
  update req `EditDetail Common.Message.TenantUpdateDetails
;;

let update_database req =
  update req `EditDatabase Common.Message.TenantUpdateDatabase
;;

let delete_asset req =
  let open Sihl.Web in
  let open Common.Message in
  let go m fcn = m |> Router.param req |> fcn in
  let asset_id = go Field.(AssetId |> show) Common.Id.of_string in
  let tenant_id = go Field.(Tenant |> show) Pool_tenant.Id.of_string in
  let redirect_path =
    Format.asprintf "root/tenants/%s" (Pool_tenant.Id.value tenant_id)
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let open Utils.Lwt_result.Infix in
    let ctx = database_label |> Pool_tenant.to_ctx in
    let event tenant =
      Cqrs_command.Pool_tenant_command.DestroyLogo.handle tenant asset_id
      |> Lwt_result.lift
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
    let destroy_file () =
      Service.Storage.delete ~ctx (Common.Id.value asset_id)
    in
    let return_to_tenant () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ FileDeleted ] ]
    in
    tenant_id
    |> Pool_tenant.find
    >>= event
    |>> handle
    |>> destroy_file
    |>> return_to_tenant
  in
  result |> HttpUtils.extract_happy_path req
;;
