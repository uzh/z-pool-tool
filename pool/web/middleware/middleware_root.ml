let from_root_only () =
  let filter handler req =
    let is_root_host =
      req
      |> Sihl.Web.Request.header "host"
      |> CCOption.map2 CCString.equal_caseless Utils.Url.public_host
      |> CCOption.value ~default:false
    in
    match is_root_host with
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
      ~ctx:(Pool_common.Utils.pool_to_ctx Pool_common.Database.root)
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
