let from_root_only () =
  let filter handler req =
    let language = Pool_common.Language.En in
    Http_utils.is_req_from_root_host req
    |> function
    | true -> handler req
    | false ->
      let html = Page.Utils.error_page_not_found language () in
      Page.Layout.create_root_layout html Pool_common.Language.En None None ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return
  in
  Rock.Middleware.create ~name:"root.only" ~filter
;;

let require_root () =
  let fail_action req =
    Http_utils.invalid_session_redirect ~login_path:"/root/login" req None
  in
  let filter handler req =
    let context = Pool_context.find req in
    let open Pool_context in
    match context with
    | Error _ -> fail_action req
    | Ok { user; _ } ->
      (match user with
       | None | Some (Contact _) | Some (Admin _) -> fail_action req
       | Some (Root _) -> handler req)
  in
  Rock.Middleware.create ~name:"user.require.root" ~filter
;;
