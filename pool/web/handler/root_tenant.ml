module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Update = Root_tenant_update

let tenants req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let open Lwt_result.Syntax in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip Option.bind Message.of_string
    in
    let csrf = Sihl.Web.Csrf.find req |> Option.get in
    let* tenant_list = Tenant.find_all () in
    Page.Root.Tenant.list csrf ~tenant_list ~message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/root/tenants" in
  let events () =
    let open Lwt.Syntax in
    let* urlencoded = Sihl.Web.Request.to_urlencoded req in
    urlencoded
    |> Cqrs_command.Tenant_command.Create.decode
    |> CCResult.map_err Utils.handle_conformist_error
    |> CCResult.flat_map Cqrs_command.Tenant_command.Create.handle
    |> CCResult.map_err (fun err -> err, error_path, [])
    |> Lwt_result.lift
  in
  let handle events =
    let ( let* ) = Lwt.bind in
    let* _ =
      Lwt_list.map_s (fun event -> Pool_event.handle_event event) events
    in
    Lwt.return_ok ()
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ "Tenant was successfully created." ] ]
  in
  ()
  |> events
  >|= handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path_with_actions
;;

let tenant_detail req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/root/tenants" in
  let show () =
    let open Lwt_result.Syntax in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip Option.bind Message.of_string
    in
    let id = Sihl.Web.Router.param req "id" in
    let* tenant = Tenant.find_by_id (id |> Pool_common.Id.of_string) in
    let csrf = Sihl.Web.Csrf.find req |> Option.get in
    Page.Root.Tenant.detail csrf ~tenant ~message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;
