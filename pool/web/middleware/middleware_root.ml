let from_root_only () =
  let filter handler req =
    Http_utils.is_req_from_root_host req
    |> function
    | true -> handler req
    | false ->
      Page.Utils.error_page_not_found ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return
  in
  Rock.Middleware.create ~name:"root.only" ~filter
;;

let require_root ~login_path_f =
  let open Utils.Lwt_result.Infix in
  let fail_action = () |> login_path_f |> Http_utils.redirect_to in
  let filter handler req =
    Service.User.Web.user_from_session
      ~ctx:(Pool_tenant.to_ctx Pool_database.root)
      req
    >|> function
    | Some user ->
      user
      |> Sihl_user.is_admin
      |> (function
      | false -> fail_action
      | true -> handler req)
    | None -> fail_action
  in
  Rock.Middleware.create ~name:"user.require.root" ~filter
;;
