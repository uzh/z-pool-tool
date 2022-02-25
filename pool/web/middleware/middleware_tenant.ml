let tenant_db_of_request req
    : (Pool_database.Label.t, Pool_common.Message.error) result Lwt.t
  =
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let open Lwt_result.Syntax in
  let* host =
    req
    |> Sihl.Web.Request.header "host"
    |> CCOption.to_result Pool_common.Message.(NotFound Host)
    |> Lwt_result.lift
  in
  (* TODO [aerben] why not direct sql here? *)
  let%lwt selections = Pool_tenant.Selection.find_all () in
  CCList.assoc_opt
    ~eq:(fun m k -> CCString.prefix ~pre:m k)
    host
    (selections
    |> CCList.map (fun sel -> Pool_tenant.Selection.(url sel, label sel)))
  |> CCOption.to_result Pool_common.Message.(NotFound TenantPool)
  |> CCResult.map_err (CCFun.const Pool_common.Message.SessionTenantNotFound)
  |> Lwt_result.lift
;;
