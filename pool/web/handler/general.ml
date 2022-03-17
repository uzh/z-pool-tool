let dashboard_path tenant_db query_lang user =
  let open Lwt.Infix in
  Admin.user_is_admin tenant_db user
  >|= (function
        | true -> "/admin/dashboard"
        | false -> "/dashboard")
  >|= Http_utils.path_with_language query_lang
;;
