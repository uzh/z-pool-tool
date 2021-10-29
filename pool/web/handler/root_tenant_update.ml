module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Common = Pool_common

let update req command success_message =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let redirect_path = Format.asprintf "/root/tenants/%s" id in
  let tenant () =
    Tenant.find_full (id |> Pool_common.Id.of_string)
    |> Lwt_result.map_err (fun err -> err, redirect_path)
  in
  let events tenant =
    let open CCResult.Infix in
    let open Lwt_result.Syntax in
    let%lwt multipart_encoded =
      Sihl.Web.Request.to_multipart_form_data_exn req
    in
    let* _ =
      File.update_files
        [ "styles", tenant.Tenant.Write.styles |> Tenant.Styles.Write.value
        ; "icon", tenant.Tenant.Write.icon |> Tenant.Icon.Write.value
        ]
        req
      |> Lwt_result.map_err (fun err -> err, redirect_path)
    in
    let* logo_files =
      File.upload_files (Tenant.LogoMapping.LogoType.all ()) req
      |> Lwt_result.map_err (fun err -> err, redirect_path)
    in
    let events_list urlencoded =
      match command with
      | `EditDetail ->
        Cqrs_command.Tenant_command.EditDetails.decode urlencoded
        |> CCResult.map_err Utils.handle_conformist_error
        >>= Cqrs_command.Tenant_command.EditDetails.handle logo_files tenant
      | `EditDatabase ->
        Cqrs_command.Tenant_command.EditDatabase.decode urlencoded
        |> CCResult.map_err Utils.handle_conformist_error
        >>= CCFun.flip Cqrs_command.Tenant_command.EditDatabase.handle tenant
    in
    multipart_encoded
    |> File.multipart_form_data_to_urlencoded
    |> HttpUtils.format_request_boolean_values [ "disabled" ]
    |> events_list
    |> CCResult.map_err (fun err -> err, redirect_path)
    |> Lwt_result.lift
  in
  let handle =
    Lwt_list.iter_s (Pool_event.handle_event Pool_common.Database.root)
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set ~success:[ success_message ] ]
  in
  ()
  |> tenant
  >>= events
  |>> handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let update_detail req =
  update req `EditDetail "Tenant was successfully updated."
;;

let update_database req =
  update req `EditDatabase "Database information was successfully updated."
;;

let delete_asset req =
  let open Utils.Lwt_result.Infix in
  let asset_id = Sihl.Web.Router.param req "asset_id" in
  let tenant_id = Sihl.Web.Router.param req "tenant_id" in
  let redirect_path = Format.asprintf "root/tenants/%s" tenant_id in
  let result () =
    let tenant () = Tenant.find (tenant_id |> Common.Id.of_string) in
    let event tenant =
      Cqrs_command.Tenant_command.DestroyLogo.handle
        tenant
        (asset_id |> Common.Id.of_string)
      |> Lwt_result.lift
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event Pool_common.Database.root)
    in
    let destroy_file () =
      Service.Storage.delete
        ~ctx:
          [ "pool", Common.Database.root |> Pool_common.Database.Label.value ]
        ~id:asset_id
    in
    let return_to_tenant =
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ "File was successfully deleted." ] ]
    in
    ()
    |> tenant
    >>= event
    |>> handle
    |>> destroy_file
    |>> CCFun.const return_to_tenant
  in
  ()
  |> result
  |> Lwt_result.map_err (fun err -> err, redirect_path)
  >|> HttpUtils.extract_happy_path
;;
