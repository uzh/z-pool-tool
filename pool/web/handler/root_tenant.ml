module Common = Pool_common
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Update = Root_tenant_update

let tenants req =
  let message =
    Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
  in
  let csrf = HttpUtils.find_csrf req in
  let%lwt tenant_list = Tenant.find_all () in
  let%lwt root_list = Root.find_all () in
  Page.Root.Tenant.list csrf tenant_list root_list message ()
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Tenant_command.Create in
  let error_path = "/root/tenants" in
  let events () =
    let open CCResult.Infix in
    let open Lwt_result.Syntax in
    let%lwt multipart_encoded =
      Sihl.Web.Request.to_multipart_form_data_exn req
    in
    let file_fields =
      [ "styles"; "icon" ] @ Tenant.LogoMapping.LogoType.all ()
    in
    let* files =
      File.upload_files file_fields req
      |> Lwt_result.map_err (fun err -> err, error_path)
    in
    let destroy_files =
      Lwt_list.map_s (fun (_, id) ->
          Service.Storage.delete
            ~ctx:
              [ "pool", Common.Database.root |> Pool_common.Database.Label.value
              ]
            ~id)
    in
    files @ multipart_encoded
    |> File.multipart_form_data_to_urlencoded
    |> decode
    >>= handle
    |> Lwt_result.lift
    |> Lwt_result.map_err (fun err ->
           let _ = destroy_files files in
           err, error_path)
  in
  let handle =
    Lwt_list.iter_s (Pool_event.handle_event Pool_common.Database.root)
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ "Tenant was successfully created." ] ]
  in
  ()
  |> events
  |>> handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let create_operator req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command.Admin_command.CreateOperator in
  let id = Sihl.Web.Router.param req "id" in
  let error_path = Format.asprintf "/root/tenants/%s" id in
  let user () =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    Sihl.Web.Request.urlencoded "email" req
    |> Lwt.map (CCOpt.to_result Pool_common.Error.EmailAddressMissingOperator)
    >>= HttpUtils.validate_email_existance tenant_db
  in
  let find_tenant () = Tenant.find_full (id |> Common.Id.of_string) in
  let events tenant =
    let open CCResult.Infix in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    urlencoded |> decode >>= handle tenant |> Lwt_result.lift
  in
  let handle events =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    Lwt_list.iter_s (Pool_event.handle_event tenant_db) events |> Lwt.return_ok
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ "Operator was successfully created." ] ]
  in
  ()
  |> user
  >>= find_tenant
  >>= events
  |>> handle
  |> Lwt_result.map_err (fun err -> err, error_path)
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let tenant_detail req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/root/tenants" in
  let show () =
    let open Lwt_result.Syntax in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
    in
    let id = Sihl.Web.Router.param req "id" in
    let* tenant = Tenant.find (id |> Common.Id.of_string) in
    let csrf = HttpUtils.find_csrf req in
    Page.Root.Tenant.detail csrf tenant message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;
