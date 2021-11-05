let tenant_db_of_request req
    : (Pool_common.Database.Label.t, Pool_common.Error.t) result Lwt.t
  =
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let db_pool () =
    let open Lwt_result.Syntax in
    let* host =
      req
      |> Sihl.Web.Request.header "host"
      |> CCOpt.to_result Pool_common.Error.(NotFound Host)
      |> Lwt_result.lift
    in
    let%lwt selections = Tenant.Selection.find_all () in
    CCList.assoc_opt
      ~eq:(fun m k -> CCString.prefix ~pre:m k)
      host
      (selections
      |> CCList.map (fun sel -> Tenant.Selection.(url sel, label sel)))
    |> CCOpt.to_result Pool_common.Error.(NotFound Tenant)
    |> Lwt_result.lift
  in
  ()
  |> db_pool
  |> Lwt_result.map_err (CCFun.const Pool_common.Error.TenantSessionNotFound)
;;
