let tenant_db_of_request req
    : (Pool_common.Database.Label.t, Pool_common.Message.error) result Lwt.t
  =
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let open Lwt_result.Syntax in
  let* host =
    req
    |> Sihl.Web.Request.header "host"
    |> CCOpt.to_result Pool_common.Message.(NotFound Host)
    |> Lwt_result.lift
  in
  let%lwt selections = Tenant.Selection.find_all () in
  CCList.assoc_opt
    ~eq:(fun m k -> CCString.prefix ~pre:m k)
    host
    (selections |> CCList.map (fun sel -> Tenant.Selection.(url sel, label sel)))
  |> CCOpt.to_result Pool_common.Message.(NotFound Tenant)
  |> CCResult.map_err (CCFun.const Pool_common.Message.SessionTenantNotFound)
  |> Lwt_result.lift
;;
