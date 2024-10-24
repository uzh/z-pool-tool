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
    match%lwt tenant_of_request req with
    | Ok ({ Pool_tenant.database_label; status; _ } as tenant) ->
      let open Database.Status in
      let handle_request =
        Settings.find_languages database_label
        ||> Tenant.create tenant
        ||> Tenant.set req
        >|> handler
      in
      (match status with
       | Active -> handle_request
       | Maintenance | MigrationsPending | MigrationsFailed ->
         let open Pool_common in
         let language = Language.En in
         let to_string = Utils.text_to_string language in
         let title = I18n.TenantMaintenanceTitle |> to_string in
         let text = I18n.TenantMaintenanceText |> to_string in
         Page.Utils.note title text
         |> Layout.Error.create
         |> Sihl.Web.Response.of_html
         |> Lwt.return
       | ConnectionIssue ->
         (match%lwt Database.connect database_label with
          | Ok () ->
            let%lwt () = Database.Tenant.update_status database_label Active in
            handle_request
          | Error _ -> Http_utils.redirect_to "/error")
       | Disabled -> Http_utils.redirect_to "/error")
    | Error err ->
      let (_ : Pool_message.Error.t) =
        Pool_common.Utils.with_log_error ~src ~tags:(Logger.Tags.req req) err
      in
      Http_utils.redirect_to "/not-found"
  in
  Rock.Middleware.create ~name:"tenant.valid" ~filter
;;
