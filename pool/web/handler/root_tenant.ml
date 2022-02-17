module Common = Pool_common
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module File = HttpUtils.File
module Update = Root_tenant_update
module Database = Pool_database

let tenants req =
  let csrf = HttpUtils.find_csrf req in
  let message =
    CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
  in
  let%lwt tenant_list = Pool_tenant.find_all () in
  let%lwt root_list = Root.find_all () in
  Page.Root.Tenant.list csrf tenant_list root_list message ()
  |> Sihl.Web.Response.of_html
  |> Lwt.return
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let events () =
    Lwt_result.map_err (fun err -> err, "/root/tenants")
    @@
    let open Lwt_result.Syntax in
    let%lwt multipart_encoded =
      Sihl.Web.Request.to_multipart_form_data_exn req
    in
    let file_fields =
      [ "styles"; "icon" ] @ Pool_tenant.LogoMapping.LogoType.all ()
    in
    let* files = File.upload_files file_fields req in
    let finalize = function
      | Ok resp -> Lwt.return_ok resp
      | Error err ->
        let ctx = Pool_tenant.to_ctx Database.root in
        let%lwt () =
          Lwt_list.iter_s (fun (_, id) -> Service.Storage.delete ~ctx id) files
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
      [ Message.set ~success:[ Common.Message.(Created Tenant) ] ]
  in
  ()
  |> events
  |>> handle
  |>> return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let create_operator req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" |> Common.Id.of_string in
  (* TODO [aerben] promote here *)
  (* let user tenant_db =
   *   Sihl.Web.Request.urlencoded "email" req
   *   ||> CCOption.to_result Common.Message.EmailAddressMissingOperator
   *   >>= HttpUtils.validate_email_existence tenant_db
   * in *)
  let find_tenant () = Pool_tenant.find_full id in
  let events tenant =
    let open Lwt_result.Syntax in
    let tenant_db = Pool_tenant.Write.(tenant.database.Database.label) in
    let* email =
      Sihl.Web.Request.urlencoded "email" req
      ||> CCOption.to_result Common.Message.EmailAddressMissingOperator
    in
    let ctx = Pool_tenant.to_ctx tenant_db in
    let%lwt user = Service.User.find_by_email_opt ~ctx email in
    let open CCResult.Infix in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    (* TODO [aerben] CONTINUE HERE *)
    let bla = Admin.find_by_email in
    let events =
      match user with
      | None ->
        let open Cqrs_command.Admin_command.CreateOperator in
        urlencoded |> decode >>= handle tenant
      | Some _ ->
        let open Cqrs_command.Admin_command.PromoteToOperator in
        urlencoded |> decode >>= handle
    in
    events >|= CCPair.make tenant_db |> Lwt_result.lift
  in
  let handle tenant_db events =
    Lwt_list.iter_s (Pool_event.handle_event tenant_db) events |> Lwt_result.ok
  in
  let return_to_overview () =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ Pool_common.Message.(Created Operator) ] ]
  in
  ()
  |> find_tenant
  >>= events
  >>= CCFun.uncurry handle
  |> Lwt_result.map_err (fun err ->
         err, Format.asprintf "/root/tenants/%s" (Common.Id.value id))
  |>> return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let tenant_detail req =
  let open Lwt_result.Syntax in
  let open Sihl.Web in
  let%lwt result =
    Lwt_result.map_err (fun err -> err, "/root/tenants")
    @@
    let csrf = HttpUtils.find_csrf req in
    let message = CCOption.bind (Flash.find_alert req) Message.of_string in
    let id = Router.param req "id" |> Common.Id.of_string in
    let* tenant = Pool_tenant.find id in
    Page.Root.Tenant.detail csrf tenant message ()
    |> Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;
