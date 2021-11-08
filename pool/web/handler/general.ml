let user_from_session req : Sihl_user.t option Lwt.t =
  let%lwt tenant_db = Middleware.Tenant.tenant_db_of_request req in
  let user db_pool =
    let ctx = Pool_common.Utils.pool_to_ctx db_pool in
    Service.User.Web.user_from_session ~ctx req
  in
  tenant_db |> CCResult.to_opt |> CCOpt.map_or ~default:Lwt.return_none user
;;
