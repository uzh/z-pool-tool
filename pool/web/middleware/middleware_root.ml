let from_root_only () =
  let open Utils.Lwt_result.Infix in
  let filter handler req =
    Http_utils.is_req_from_root_host req
    |> function
    | true -> handler req
    | false ->
      let context =
        let open Pool_context in
        let csrf = Sihl.Web.Csrf.find_exn req in
        create
          ( []
          , Pool_common.Language.En
          , Database.Pool.Root.label
          , None
          , csrf
          , Guest
          , []
          , [] )
      in
      Page.Utils.error_page_not_found Pool_common.Language.En ()
      |> Layout.Root.create context
      ||> Sihl.Web.Response.of_html
  in
  Rock.Middleware.create ~name:"root.only" ~filter
;;

let require_root () =
  let fail_action req =
    Http_utils.invalid_session_redirect
      ~login_path:(CCFun.flip Http_utils.intended_of_request "/root/login")
      req
      []
  in
  let filter handler req =
    let context = Pool_context.find req in
    let open Pool_context in
    match context with
    | Error _ -> fail_action req
    | Ok { user; _ } ->
      (match user with
       | Contact _ | Guest -> fail_action req
       | Admin _ -> handler req)
  in
  Rock.Middleware.create ~name:"user.require.root" ~filter
;;
