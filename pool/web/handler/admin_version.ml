open Pool_message

let src = Logs.Src.create "handler.admin.version"
let version_path = Http_utils.Url.Admin.version_path
let active_navigation = version_path ()
let create_layout req = General.create_tenant_layout req

let version_id req =
  Http_utils.get_field_router_param req Field.Version |> Pool_version.Id.of_string
;;

let index req =
  Http_utils.Htmx.handler
    ~active_navigation
    ~error_path:"dashboard"
    ~query:(module Pool_version)
    ~create_layout
    req
  @@ fun context query ->
  let%lwt versions = Pool_version.all_on_tenant ~query () in
  let open Page.Admin.Version in
  (if Http_utils.Htmx.is_hx_request req then list else index) context versions
  |> Lwt_result.return
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, version_path ())
    @@
    let* version = req |> version_id |> Pool_version.find in
    Page.Admin.Version.show context version
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path ~src req
;;
