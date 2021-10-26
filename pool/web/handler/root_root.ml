module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create req =
  let open Utils.Lwt_result.Infix in
  let error_path = Format.asprintf "/root/tenants/" in
  let user () =
    let open Lwt_result.Syntax in
    let%lwt email_address = Sihl.Web.Request.urlencoded "email" req in
    let* tenant_db = Middleware.Tenant_middleware.tenant_db_of_request req in
    email_address
    |> CCOpt.to_result "Please provide root email address."
    |> Lwt_result.lift
    >>= HttpUtils.user_email_exists tenant_db
  in
  let events () =
    let open CCResult.Infix in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    urlencoded
    |> Cqrs_command.Root_command.Create.decode
    |> CCResult.map_err Utils.handle_conformist_error
    >>= Cqrs_command.Root_command.Create.handle
    |> Lwt_result.lift
  in
  let handle =
    Lwt_list.iter_s (Pool_event.handle_event Pool_common.Database.root)
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ "Root was successfully created." ] ]
  in
  ()
  |> user
  >>= events
  |>> handle
  |> Lwt_result.map_err (fun err -> err, error_path)
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let toggle_status req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let error_path = Format.asprintf "/root/tenants/" in
  let events user =
    Cqrs_command.Root_command.ToggleStatus.handle user |> Lwt_result.lift
  in
  let handle =
    Lwt_list.iter_s (Pool_event.handle_event Pool_common.Database.root)
  in
  let return_to_overview =
    Http_utils.redirect_to_with_actions
      "/root/tenants"
      [ Message.set ~success:[ "Root was successfully updated." ] ]
  in
  id
  |> Pool_common.Id.of_string
  |> Root.find
  >>= events
  |>> handle
  |> Lwt_result.map_err (fun err -> err, error_path)
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;
