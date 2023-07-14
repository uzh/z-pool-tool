module HttpUtils = Http_utils
module Message = HttpUtils.Message

let require_admin () =
  let fail_action req = Http_utils.invalid_session_redirect req None in
  let filter handler req =
    let context = Pool_context.find req in
    let open Pool_context in
    match context with
    | Error _ -> fail_action req
    | Ok { user; _ } ->
      (match user with
       | Admin { Admin.import_pending; _ } ->
         (match Pool_user.ImportPending.value import_pending with
          | true ->
            HttpUtils.redirect_to_with_actions
              "/import-pending"
              [ Sihl.Web.Session.set [ "user_id", "" ] ]
          | false -> handler req)
       | Contact _ | Guest -> fail_action req)
  in
  Rock.Middleware.create ~name:"user.require.admin" ~filter
;;
