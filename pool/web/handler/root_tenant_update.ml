module HttpUtils = Http_utils
module Message = HttpUtils.Message

let update req command message =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let redirect_path = Format.asprintf "/root/tenant/%s" id in
  let map_err err = err, redirect_path, [] in
  let tenant () =
    Tenant.find_full_by_id id |> Lwt_result.map_err (fun err -> map_err err)
  in
  let events tenant =
    let open Lwt.Syntax in
    let* urlencoded = Sihl.Web.Request.to_urlencoded req in
    let events_list urlencoded =
      match command with
      | `EditDetail ->
        Cqrs_command.Tenant_command.EditDetails.decode urlencoded
        |> CCResult.map_err Utils.handle_conformist_error
        |> CCResult.flat_map
             (CCFun.flip Cqrs_command.Tenant_command.EditDetails.handle tenant)
      | `EditDatabase ->
        Cqrs_command.Tenant_command.EditDatabase.decode urlencoded
        |> CCResult.map_err Utils.handle_conformist_error
        |> CCResult.flat_map
             (CCFun.flip Cqrs_command.Tenant_command.EditDatabase.handle tenant)
    in
    urlencoded
    |> events_list
    |> CCResult.map_err (fun err -> map_err err)
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
      redirect_path
      [ Message.set ~success:[ message ] ]
  in
  ()
  |> tenant
  >>= events
  >|= handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path_with_actions
;;

let update_detail req =
  update req `EditDetail "Tenant was successfully updated."
;;

let update_database req =
  update req `EditDatabase "Database information was successfully updated."
;;
