module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout req

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let%lwt admin_users = Admin.find_all tenant_db () in
       Page.Admin.Admins.index context admin_users
       |> create_layout req ~active_navigation:"/admin/admins" context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let admin_detail req is_edit =
  (* TODO: Impelement authorization *)
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Admin
      |> Pool_common.Id.of_string
    in
    let* admin = id |> Admin.find_any_admin_by_user_id tenant_db in
    let* () =
      let* _ = General.admin_from_session tenant_db req in
      Lwt.return_ok ()
    in
    (match is_edit with
     | true -> Page.Admin.Admins.edit context admin
     | false -> Page.Admin.Admins.detail context admin)
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail req = admin_detail req false
let edit req = admin_detail req true
