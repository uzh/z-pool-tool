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
  let open Utils.Lwt_result.Infix in
  let denied = Pool_message.Error.AccessDenied in
  let find_api_key database_label req =
    let open CCOption in
    let open Opium in
    let key = "X-AUTH-TOKEN" in
    Headers.get req.Request.headers key
    |> map_or ~default:(Lwt.return None) (Api_key.find_by_token database_label)
    ||> to_result denied
  in
  let filter handler req =
    let is_root = Http_utils.is_req_from_root_host req in
    let%lwt context =
      let with_status status = CCResult.map_err (fun err -> err, status) in
      let* database_label =
        Middleware_context.database_label_of_request is_root req
        |> with_status `Internal_server_error
        |> Lwt_result.lift
      in
      let* api_key =
        find_api_key database_label req ||> with_status `Unauthorized
      in
      let%lwt guardian =
        api_key.Api_key.id
        |> Guard.Uuid.actor_of Api_key.Id.value
        |> Guard.Persistence.ActorRole.permissions_of_actor database_label
      in
      create api_key.Api_key.id database_label guardian |> Lwt_result.return
    in
    match context with
    | Ok context -> context |> set req |> handler
    | Error (err, status) -> respond_error status err
  in
  Rock.Middleware.create ~name:"api.context" ~filter
;;
