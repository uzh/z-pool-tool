open Utils.Lwt_result.Infix

let src = Logs.Src.create "middleware.tenant"

let tenant_url_of_request =
  let open CCFun in
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  Sihl.Web.Request.header "host"
  %> CCOption.to_result Pool_message.(Error.NotFound Field.Host)
  %> flip CCResult.( >>= ) Pool_tenant.Url.create
;;

let tenant_of_request req =
  tenant_url_of_request req |> Lwt_result.lift >>= Pool_tenant.find_by_url
;;

let validate () =
  let filter handler req =
    let open Pool_context in
    match Tenant.find req with
    | Ok context ->
      (* TODO: add test *)
      context |> Tenant.set req |> handler
    | Error _ ->
      (match%lwt tenant_of_request req with
       | Ok tenant when Database.Status.(equal Active) tenant.Pool_tenant.status
         ->
         Settings.find_languages tenant.Pool_tenant.database_label
         ||> Tenant.create tenant
         ||> Tenant.set req
         >|> handler
       | Ok _ ->
         let (_ : Pool_message.Error.t) =
           Pool_common.Utils.with_log_error
             ~src
             ~tags:(Logger.Tags.req req)
             Pool_message.(Error.NotFound Field.Tenant)
         in
         Http_utils.redirect_to "/error"
       | Error err ->
         let (_ : Pool_message.Error.t) =
           Pool_common.Utils.with_log_error ~src ~tags:(Logger.Tags.req req) err
         in
         Http_utils.redirect_to "/not-found")
  in
  Rock.Middleware.create ~name:"tenant.valid" ~filter
;;
