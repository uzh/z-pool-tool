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
       | None | Some (Contact _) | Some (Root _) -> fail_action req
       | Some (Admin _) -> handler req)
  in
  Rock.Middleware.create ~name:"user.require.admin" ~filter
;;
