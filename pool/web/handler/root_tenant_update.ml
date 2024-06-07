module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Common = Pool_common
module Database = Database
module Conformist = Pool_conformist

let src = Logs.Src.create "handler.root.tenant_update"

let validated_gtx_api_key ~tags title urlencoded =
  let open Utils.Lwt_result.Infix in
  let schema =
    Conformist.(
      make
        Field.[ Pool_tenant.GtxApiKey.schema (); Pool_user.CellPhone.schema () ]
        CCPair.make)
  in
  Conformist.decode_and_validate schema urlencoded
  |> Lwt_result.lift
  >|- Pool_message.to_conformist_error
  >>= fun (api_key, phone_nr) ->
  Text_message.Service.test_api_key ~tags api_key phone_nr title
;;

let update req command success_message =
  let open Utils.Lwt_result.Infix in
  let open Pool_message.Field in
  let open Pool_tenant in
  let%lwt multipart_encoded =
    Sihl.Web.Request.to_multipart_form_data_exn req
    ||> HttpUtils.remove_empty_values_multiplart
  in
  let urlencoded =
    multipart_encoded |> HttpUtils.multipart_to_urlencoded file_fields
  in
  let tags = Pool_context.Logger.Tags.req req in
  let id =
    HttpUtils.get_field_router_param req tenant |> Pool_tenant.Id.of_string
  in
  let redirect_path =
    Format.asprintf "/root/tenants/%s" (Pool_tenant.Id.value id)
  in
  let result (_ : Pool_context.t) =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let events tenant_model =
      let open Utils.Lwt_result.Infix in
      let open Pool_tenant in
      let updates, creations =
        (CCList.fold_left (fun (updates, creations) (asset_id, field) ->
           match asset_id with
           | Some id -> (show field, id) :: updates, creations
           | None -> updates, field :: creations))
          ([], [])
          CCOption.
            [ tenant_model.Write.styles >|= Styles.Write.value, Styles
            ; tenant_model.Write.icon >|= Icon.Write.value, Icon
            ]
      in
      let* (_ : string list) = File.update_files Database.root updates req in
      let* uploaded_files =
        match creations with
        | [] -> Lwt_result.return []
        | fields -> File.upload_files Database.root (CCList.map show fields) req
      in
      let* logo_files =
        File.upload_files
          Database.root
          (Pool_tenant.LogoMapping.LogoType.all_fields |> CCList.map show)
          req
      in
      let events_list urlencoded =
        let open Cqrs_command.Pool_tenant_command in
        let lift = Lwt_result.lift in
        match command with
        | `EditDetail ->
          let open CCResult.Infix in
          EditDetails.(decode urlencoded >>= handle ~tags tenant_model) |> lift
        | `EditDatabase ->
          let open UpdateDatabase in
          let* { database_url; database_label } = decode urlencoded |> lift in
          let* database =
            Database.test_and_create database_url database_label
          in
          handle ~tags tenant_model database |> lift
      in
      let files = logo_files @ uploaded_files in
      (files |> File.multipart_form_data_to_urlencoded) @ urlencoded
      |> HttpUtils.format_request_boolean_values [ TenantDisabledFlag |> show ]
      |> events_list
      >|> HttpUtils.File.cleanup_upload Database.root files
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags Database.root)
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success_message ] ]
    in
    id |> Pool_tenant.find_full >>= events |>> handle |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let update_detail req =
  update req `EditDetail Pool_message.Success.TenantUpdateDetails
;;

let update_database req =
  update req `EditDatabase Pool_message.Success.TenantUpdateDatabase
;;

let delete_asset req =
  let open Sihl.Web in
  let open Pool_message in
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
    let event tenant =
      Cqrs_command.Pool_tenant_command.DestroyLogo.handle tenant asset_id
      |> Lwt_result.lift
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
    let destroy_file () =
      Storage.delete database_label (Common.Id.value asset_id)
    in
    let return_to_tenant () =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Success.FileDeleted ] ]
    in
    tenant_id
    |> Pool_tenant.find
    >>= event
    |>> handle
    |>> destroy_file
    |>> return_to_tenant
  in
  result |> HttpUtils.extract_happy_path ~src req
;;
