module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Common = Pool_common
module Database = Pool_database

let update req command success_message =
  let result _ =
    let open Utils.Lwt_result.Infix in
    let id = Sihl.Web.Router.param req "id" |> Common.Id.of_string in
    let redirect_path =
      Format.asprintf "/root/tenants/%s" (Common.Id.value id)
    in
    let events tenant =
      let open Lwt_result.Syntax in
      let%lwt multipart_encoded =
        Sihl.Web.Request.to_multipart_form_data_exn req
      in
      let* _ =
        File.update_files
          [ ( "styles"
            , tenant.Pool_tenant.Write.styles |> Pool_tenant.Styles.Write.value
            )
          ; ( "icon"
            , tenant.Pool_tenant.Write.icon |> Pool_tenant.Icon.Write.value )
          ]
          req
      in
      let* logo_files =
        File.upload_files
          |> CCList.map Pool_common.Message.field_name)
          (Pool_tenant.LogoMapping.LogoType.all_fields
          req
      in
      let events_list urlencoded =
        let open CCResult.Infix in
        match command with
        | `EditDetail ->
          Cqrs_command.Pool_tenant_command.EditDetails.(
            decode urlencoded >>= handle tenant)
        | `EditDatabase ->
          Cqrs_command.Pool_tenant_command.EditDatabase.(
            decode urlencoded >>= handle tenant)
      in
      logo_files @ multipart_encoded
      |> File.multipart_form_data_to_urlencoded
      |> HttpUtils.format_request_boolean_values [ "disabled" ]
      |> events_list
      |> Lwt_result.lift
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success_message ] ]
    in
    id
    |> Pool_tenant.find_full
    >>= events
    |> Lwt_result.map_err (fun err -> err, redirect_path)
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
  let asset_id = Router.param req "asset_id" |> Common.Id.of_string in
  let tenant_id = Router.param req "tenant_id" |> Common.Id.of_string in
  let redirect_path =
    Format.asprintf "root/tenants/%s" (Common.Id.value tenant_id)
  in
  let result context =
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let open Utils.Lwt_result.Infix in
    let ctx = context.Pool_context.tenant_db |> Pool_tenant.to_ctx in
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
        [ Message.set ~success:[ Pool_common.Message.FileDeleted ] ]
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
