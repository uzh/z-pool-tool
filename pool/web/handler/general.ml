let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  let ctx = Pool_tenant.to_ctx db_pool in
  Service.User.Web.user_from_session ~ctx req
;;

let admin_from_session db_pool req =
  let open Utils.Lwt_result.Infix in
  user_from_session db_pool req
  ||> CCOption.to_result Pool_common.Message.(NotFound User, "/login")
  >>= fun user ->
  user.Sihl.Contract.User.id
  |> Pool_common.Id.of_string
  |> Admin.find_any_admin_by_user_id db_pool
  |> Lwt_result.map_err (fun err -> err, "/login")
;;

let dashboard_path tenant_db user =
  let open Lwt.Infix in
  Admin.user_is_admin tenant_db user
  >|= function
  | true -> "/admin/dashboard"
  | false -> "/dashboard"
;;
