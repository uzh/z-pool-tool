let dashboard_path tenant_db user =
  let open Lwt.Infix in
  Admin.user_is_admin tenant_db user
  >|= function
  | true -> "/admin/dashboard"
  | false -> "/dashboard"
;;

let create_tenant_layout
    layout_context
    req
    Pool_context.{ language; message; _ }
    children
  =
  let open Lwt_result.Syntax in
  let* tenant_context = Pool_context.Tenant.find req |> Lwt_result.lift in
  Page.Layout.Tenant.create_layout
    layout_context
    children
    tenant_context
    message
    language
  |> Lwt_result.return
;;
