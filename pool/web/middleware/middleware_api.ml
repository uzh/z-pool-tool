let src = Logs.Src.create "middleware.api_tenant"

let respond_error status error =
  Http_utils.Api.respond_error ~status error |> Lwt.return
;;

let validate_tenant () =
  let open Pool_message in
  let maintenance_handler () =
    respond_error `Service_unavailable Error.MaintenancePending
  in
  let connection_issue_handler () =
    respond_error `Service_unavailable Error.ServiceUnavailable
  in
  let error_handler err = respond_error `Internal_server_error err in
  Middleware_tenant.make
    "api.tenant.valid"
    ~maintenance_handler
    ~connection_issue_handler
    ~error_handler
    ()
;;

let context () =
  let open Pool_context.Api in
  let api_key = "super_safe_api_key" in
  let find_api_key req =
    let open Opium in
    let open CCOption.Infix in
    let key = "X-AUTH-TOKEN" in
    Headers.get req.Request.headers key
    >>= (fun token -> if token = api_key then Some token else None)
    |> CCOption.to_result Pool_message.Error.AccessDenied
  in
  let filter handler req =
    let is_root = Http_utils.is_req_from_root_host req in
    let context =
      let open CCResult in
      let with_status status = map_err (fun err -> err, status) in
      let* database_label =
        Middleware_context.database_label_of_request is_root req
        |> with_status `Internal_server_error
      in
      let* api_key = find_api_key req |> with_status `Unauthorized in
      (* TODO *)
      let guardian = [] in
      create api_key database_label guardian |> return
    in
    match context with
    | Ok context -> context |> set req |> handler
    | Error (err, status) -> respond_error status err
  in
  Rock.Middleware.create ~name:"api.context" ~filter
;;
