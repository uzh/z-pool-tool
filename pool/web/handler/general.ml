let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  let ctx = Pool_tenant.to_ctx db_pool in
  Service.User.Web.user_from_session ~ctx req
;;

let dashboard_path tenant_db query_lang user =
  let open Lwt.Infix in
  Admin.user_is_admin tenant_db user
  >|= (function
        | true -> "/admin/dashboard"
        | false -> "/dashboard")
  >|= Http_utils.path_with_lang query_lang
;;
