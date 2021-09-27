module HttpUtils = Http_utils
module Message = HttpUtils.Message

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
    (* QUESTION [timhub]: this is a string nod an Commin.Id *)
    let* tenant = Tenant.find_by_id id in
    let csrf = Sihl.Web.Csrf.find req |> Option.get in
    Page.Root.Tenant.detail csrf ~tenant ~message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  show ()
  |> Lwt_result.map_err (fun err -> err, error_path)
  >|> HttpUtils.extract_happy_path
;;

let update_detail req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let redirect_path = Format.asprintf "/root/tenant/%s" id in
  let events () =
    let open Lwt_result.Syntax in
    let map_err err = err, redirect_path, [] in
    let* urlencoded = Sihl.Web.Request.to_urlencoded req |> Lwt_result.ok in
    let* tenant =
      Tenant.find_full_by_id id |> Lwt_result.map_err (fun err -> map_err err)
    in
    urlencoded
    |> Cqrs_command.Tenant_command.EditDetails.decode
    |> CCResult.map_err Utils.handle_conformist_error
    |> CCResult.flat_map
         (CCFun.flip Cqrs_command.Tenant_command.EditDetails.handle tenant)
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
      [ Message.set ~success:[ "Tenant was successfully updated." ] ]
  in
  ()
  |> events
  >|= handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path_with_actions
;;

let update_database req =
  let open Utils.Lwt_result.Infix in
  let id = Sihl.Web.Router.param req "id" in
  let redirect_path = Format.asprintf "/root/tenant/%s" id in
  let events () =
    let open Lwt_result.Syntax in
    let map_err err = err, redirect_path, [] in
    let* urlencoded = Sihl.Web.Request.to_urlencoded req |> Lwt_result.ok in
    let* tenant =
      Tenant.find_full_by_id id |> Lwt_result.map_err (fun err -> map_err err)
    in
    urlencoded
    |> Cqrs_command.Tenant_command.EditDatabase.decode
    |> CCResult.map_err Utils.handle_conformist_error
    |> CCResult.flat_map
         (CCFun.flip Cqrs_command.Tenant_command.EditDatabase.handle tenant)
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
      [ Message.set
          ~success:[ "Database information was successfully updated." ]
      ]
  in
  ()
  |> events
  >|= handle
  |>> CCFun.const return_to_overview
  >|> HttpUtils.extract_happy_path_with_actions
;;
