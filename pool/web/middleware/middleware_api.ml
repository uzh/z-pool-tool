let src = Logs.Src.create "middleware.api_tenant"

let validate_tenant () =
  let open Pool_message in
  let open Http_utils.Api in
  let respond status error = respond_error ~status error |> Lwt.return in
  let maintenance_handler () =
    respond `Service_unavailable Error.MaintenancePending
  in
  let connection_issue_handler () =
    respond `Service_unavailable Error.ServiceUnavailable
  in
  let error_handler err = respond `Internal_server_error err in
  Middleware_tenant.make
    "api.tenant.valid"
    ~maintenance_handler
    ~connection_issue_handler
    ~error_handler
    ()
;;
