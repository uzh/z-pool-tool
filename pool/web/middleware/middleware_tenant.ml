let src = Logs.Src.create "middleware.tenant"

let tenant_of_request req =
  let open Utils.Lwt_result.Infix in
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let* host =
    let open CCResult in
    req
    |> Sihl.Web.Request.header "host"
    |> CCOption.to_result Pool_message.(Error.NotFound Field.Host)
    >>= Pool_tenant.Url.create
    |> Lwt_result.lift
  in
  Pool_tenant.find_by_url host
;;

let valid_tenant () =
  let open Utils.Lwt_result.Infix in
  let filter handler req =
    match Pool_context.Tenant.find req with
    | Ok context -> context |> Pool_context.Tenant.set req |> handler
    | Error _ ->
      (match%lwt tenant_of_request req with
       | Ok tenant ->
         Settings.find_languages tenant.Pool_tenant.database_label
         ||> Pool_context.Tenant.create tenant
         ||> Pool_context.Tenant.set req
         >|> handler
       | Error err ->
         let (_ : Pool_message.Error.t) =
           Pool_common.Utils.with_log_error
             ~src
             ~tags:(Pool_context.Logger.Tags.req req)
             err
         in
         Http_utils.redirect_to "/not-found")
  in
  Rock.Middleware.create ~name:"tenant.valid" ~filter
;;

let tenant_database_connection () =
  let open Utils.Lwt_result.Infix in
  let filter handler req =
    match Pool_context.Tenant.find req with
    | Ok context ->
      (* TODO: add test *)
      context |> Pool_context.Tenant.set req |> handler
    | Error _ ->
      (match%lwt tenant_of_request req with
       | Ok tenant ->
         Settings.find_languages tenant.Pool_tenant.database_label
         ||> Pool_context.Tenant.create tenant
         ||> Pool_context.Tenant.set req
         >|> handler
       | Error err ->
         let (_ : Pool_message.Error.t) =
           Pool_common.Utils.with_log_error
             ~src
             ~tags:(Pool_context.Logger.Tags.req req)
             err
         in
         Http_utils.redirect_to "/not-found")
  in
  Rock.Middleware.create ~name:"tenant.database_connected" ~filter
;;
