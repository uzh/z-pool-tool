let src = Logs.Src.create "handler.general"
let user_from_session = Pool_user.Web.user_from_session

let admin_from_session db_pool req =
  let open Utils.Lwt_result.Infix in
  user_from_session db_pool req
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.User)
  >>= fun user ->
  user.Sihl.Contract.User.id |> Admin.Id.of_string |> Admin.find db_pool
;;

let create_tenant_layout req ?active_navigation context children =
  let open Utils.Lwt_result.Infix in
  let* tenant_context = Pool_context.Tenant.find req |> Lwt_result.lift in
  Layout.Tenant.create ?active_navigation context tenant_context children
  |> Lwt_result.ok
;;

let create_root_layout ?active_navigation context =
  Layout.Root.create ?active_navigation context
;;

let note ~title ~body req =
  let result ({ Pool_context.language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/")
    @@
    let txt_to_string m = Pool_common.Utils.text_to_string language m in
    Page.Utils.note (txt_to_string title) (txt_to_string body)
    |> create_tenant_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path ~src req
;;
