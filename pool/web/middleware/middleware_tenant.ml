let tenant_of_request req =
  let open Utils.Lwt_result.Infix in
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let* host =
    req
    |> Sihl.Web.Request.header "host"
    |> CCOption.to_result Pool_common.Message.(NotFound Field.Host)
    |> Lwt_result.lift
  in
  let%lwt selections = Pool_tenant.Selection.find_all () in
  CCList.assoc_opt
    ~eq:(fun m -> CCString.prefix ~pre:m)
    host
    (selections
     |> CCList.map (fun sel -> Pool_tenant.Selection.(url sel, label sel)))
  |> CCOption.to_result Pool_common.Message.SessionTenantNotFound
  |> Lwt_result.lift
  >>= Pool_tenant.find_by_label
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
         let (_ : Pool_common.Message.error) =
           Pool_common.Utils.with_log_error
             ~tags:(Pool_context.Logger.Tags.req req)
             err
         in
         Http_utils.redirect_to "/not-found")
  in
  Rock.Middleware.create ~name:"tenant.valid" ~filter
;;
