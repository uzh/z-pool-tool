module HttpUtils = Http_utils
module Message = HttpUtils.Message

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/" in
  let show () =
    let open Lwt_result.Syntax in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip Option.bind Message.of_string
    in
    let csrf = Sihl.Web.Csrf.find req |> Option.get in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt translation_list = I18n.find_all tenant_db () in
    Page.Admin.I18n.list csrf translation_list message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let redirect_path = Format.asprintf "/admin/i18n" in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let property () = I18n.find tenant_db (id |> Pool_common.Id.of_string) in
    let events property =
      let open CCResult.Infix in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> Cqrs_command.I18n_command.Edit.decode
      >>= Cqrs_command.I18n_command.Edit.handle property
      |> Lwt_result.lift
    in
    let handle events =
      Lwt_list.map_s (Pool_event.handle_event tenant_db) events
    in
    () |> property >>= events |>> handle
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set ~success:[ Pool_common.Message.(Updated I18n) ] ]
  in
  result
  |> Lwt_result.lift
  |> Lwt_result.map_err (fun err -> err, redirect_path)
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;
