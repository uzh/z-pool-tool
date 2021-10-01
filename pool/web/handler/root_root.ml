module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create req =
  let open Utils.Lwt_result.Infix in
  let error_path = Format.asprintf "/root/tenants/" in
  let user () =
    let open Lwt.Syntax in
    let* email_address = Sihl.Web.Request.urlencoded "email" req in
    email_address
    |> CCOpt.to_result "Please provide root email address."
    |> Lwt_result.lift
    >>= HttpUtils.user_email_exists
  in
  let events () =
    let open Lwt.Syntax in
    let* urlencoded = Sihl.Web.Request.to_urlencoded req in
    urlencoded
    |> Cqrs_command.Admin_command.CreateRoot.decode
    |> CCResult.map_err Utils.handle_conformist_error
    |> CCResult.flat_map Cqrs_command.Admin_command.CreateRoot.handle
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
      [ Message.set ~success:[ "Root was successfully created." ] ]
  in
  ()
  |> user
  >>= events
  >|= handle
  |> Lwt_result.map_err (fun err -> err, error_path)
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;

let toggle_status req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let error_path = Format.asprintf "/root/tenants/" in
  let user = Admin.find_root_by_id in
  let events user =
    Cqrs_command.Admin_command.ToggleRootStatus.handle user |> Lwt_result.lift
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
      [ Message.set ~success:[ "Root was successfully updated." ] ]
  in
  id
  |> Pool_common.Id.of_string
  |> user
  >>= events
  >|= handle
  |> Lwt_result.map_err (fun err -> err, error_path)
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path
;;
