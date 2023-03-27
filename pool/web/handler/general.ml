let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  let ctx = Pool_database.to_ctx db_pool in
  Service.User.Web.user_from_session ~ctx req
;;

let admin_from_session db_pool req =
  let open Utils.Lwt_result.Infix in
  user_from_session db_pool req
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
  >>= fun user ->
  user.Sihl.Contract.User.id |> Admin.Id.of_string |> Admin.find db_pool
;;

let create_tenant_layout
  req
  ?active_navigation
  Pool_context.{ database_label; language; query_language; message; user; _ }
  children
  =
  let open Utils.Lwt_result.Infix in
  let* tenant_context = Pool_context.Tenant.find req |> Lwt_result.lift in
  Layout.create
    children
    tenant_context
    ?active_navigation
    ?message
    ?query_language
    database_label
    language
    user
  |> Lwt_result.ok
;;

let create_root_layout
  ?active_navigation
  Pool_context.{ database_label; message; user; _ }
  =
  Layout.Root.create ?active_navigation ?message database_label user
;;
