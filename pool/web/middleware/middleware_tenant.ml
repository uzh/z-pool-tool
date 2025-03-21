open Utils.Lwt_result.Infix
open Database

let src = Logs.Src.create "middleware.tenant"

(* let tenant_url_of_request =
   let open CCFun in
   (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same host cannot
   be handled at the moment *)
   Sihl.Web.Request.header "host"
   %> CCOption.to_result Pool_message.(Error.NotFound Field.Host)
   %> flip CCResult.( >>= ) Pool_tenant.Url.create
   ;; *)

let tenant_url_of_request _ = Pool_tenant.Url.create "localhost:3017"

let tenant_of_request req =
  let should_cache ({ Pool_tenant.status; _ } : Pool_tenant.t) =
    let open Status in
    match status with
    | Active | Disabled | Maintenance | MigrationsFailed -> true
    | ConnectionIssue | MigrationsConnectionIssue | MigrationsPending -> false
  in
  tenant_url_of_request req |> Lwt_result.lift >>= Pool_tenant.find_by_url ~should_cache
;;

let filter ~maintenance_handler ~connection_issue_handler ~error_handler handler req =
  let open Pool_context in
  let open Status in
  match%lwt tenant_of_request req with
  | Ok ({ Pool_tenant.database_label; status; _ } as tenant) ->
    let handle_request =
      Settings.find_languages database_label
      ||> Tenant.create tenant
      ||> Tenant.set req
      >|> handler
    in
    (match status with
     | Active -> handle_request
     | Maintenance | MigrationsConnectionIssue | MigrationsFailed | MigrationsPending ->
       maintenance_handler ()
     | Disabled -> connection_issue_handler ()
     | ConnectionIssue ->
       (match%lwt Pool.connect database_label with
        | Ok () ->
          let%lwt () =
            let open Pool_database in
            StatusUpdated (database_label, Status.Active) |> handle_event Pool.Root.label
          in
          handle_request
        | Error err -> error_handler err))
  | Error err ->
    Pool_common.Utils.with_log_error ~src ~tags:(Logger.Tags.req req) err |> error_handler
;;

let web_filter handler req =
  let maintenance_handler () =
    let open Pool_common in
    let language = Language.En in
    let to_string = Utils.text_to_string language in
    let title = I18n.TenantMaintenanceTitle |> to_string in
    let text = I18n.TenantMaintenanceText |> to_string in
    Page.Utils.note title text
    |> Layout.Error.create
    |> Sihl.Web.Response.of_html
    |> Lwt.return
  in
  let connection_issue_handler () = Http_utils.redirect_to "/error" in
  let error_handler (_ : Pool_message.Error.t) = Http_utils.redirect_to "/not-found" in
  filter ~maintenance_handler ~connection_issue_handler ~error_handler handler req
;;

let make name ~maintenance_handler ~connection_issue_handler ~error_handler () =
  let filter = filter ~maintenance_handler ~connection_issue_handler ~error_handler in
  Rock.Middleware.create ~name ~filter
;;

let validate () = Rock.Middleware.create ~name:"tenant.valid" ~filter:web_filter
