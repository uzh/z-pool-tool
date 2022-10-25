let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  let ctx = Pool_tenant.to_ctx db_pool in
  Service.User.Web.user_from_session ~ctx req
;;

let admin_from_session db_pool req =
  let open Utils.Lwt_result.Infix in
  user_from_session db_pool req
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
  >>= fun user ->
  user.Sihl.Contract.User.id
  |> Pool_common.Id.of_string
  |> Admin.find_any_admin_by_user_id db_pool
;;

let dashboard_path tenant_db user =
  let open Lwt.Infix in
  Admin.user_is_admin tenant_db user
  >|= function
  | true -> "/admin/dashboard"
  | false -> "/dashboard"
;;

let create_tenant_layout
  req
  ?active_navigation
  Pool_context.{ language; query_language; message; user; _ }
  children
  =
  let open Lwt_result.Syntax in
  let* tenant_context = Pool_context.Tenant.find req |> Lwt_result.lift in
  Page.Layout.Tenant.create_layout
    children
    tenant_context
    user
    message
    language
    query_language
    active_navigation
  |> Lwt_result.return
;;

let create_root_layout
  ?active_navigation
  Pool_context.{ language; message; user; _ }
  children
  =
  Page.Layout.create_root_layout
    children
    language
    message
    user
    ?active_navigation
    ()
;;
