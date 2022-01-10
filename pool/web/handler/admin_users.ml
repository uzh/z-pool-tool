module HttpUtils = Http_utils
module Message = HttpUtils.Message

let list_admins req =
  let%lwt result =
    Lwt_result.map_err (fun err -> err, "/admin/dashboard")
    @@
    let open Lwt_result.Syntax in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt admin_users = Admin.(find_all tenant_db ()) in
    Page.Admin.Users.list_admins admin_users message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let admin_detail req is_edit =
  let%lwt result =
    let open Lwt_result.Syntax in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let* tenant_db =
      Middleware.Tenant.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, "/admin/admins")
    in
    let* admin = General.admin_from_session tenant_db req in
    let roles_authorized_to_edit = Admin.roles_authorized_to_edit admin in
    let* editable_admin =
      Sihl.Web.Router.param req "id"
      |> Pool_common.Id.of_string
      |> Admin.find_any_admin_by_user_id tenant_db
      |> Lwt_result.map_err (fun err -> err, "/admin/admins")
    in
    (match is_edit with
    | true ->
      Page.Admin.Users.edit_admin
        roles_authorized_to_edit
        editable_admin
        message
        ()
    | false ->
      Page.Admin.Users.show_admin
        roles_authorized_to_edit
        editable_admin
        message
        ())
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let show_admin req = admin_detail req false
let edit_admin req = admin_detail req true

let list_participants req =
  let%lwt result =
    Lwt_result.map_err (fun err -> err, "/admin/dashboard")
    @@
    let open Lwt_result.Syntax in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt participants = Participant.find_all tenant_db () in
    Page.Admin.Users.list_participants participants message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;

let show_participant req =
  let%lwt result =
    Lwt_result.map_err (fun err -> err, "/admin/dashboard")
    @@
    let open Lwt_result.Syntax in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
    in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let* participant =
      Sihl.Web.Router.param req "id"
      |> Pool_common.Id.of_string
      |> Participant.find tenant_db
    in
    Page.Admin.Users.show_participant participant message ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path
;;
