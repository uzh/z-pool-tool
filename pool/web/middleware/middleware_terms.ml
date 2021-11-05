module Message = Http_utils.Message

let[@warning "-4"] terms_accepted () =
  let filter handler req =
    let%lwt has_accepted =
      let open Lwt_result.Syntax in
      let* user_id =
        req
        |> Sihl.Web.Session.find "user_id"
        |> CCOpt.map Pool_common.Id.of_string
        |> CCOpt.to_result Pool_common.Error.(NotFound User)
        |> Lwt_result.lift
      in
      let* pool = Middleware_tenant.tenant_db_of_request req in
      let* participant = Participant.find pool user_id in
      Participant.has_terms_accepted participant |> Lwt_result.ok
    in
    match has_accepted with
    | Ok true -> handler req
    | Ok false ->
      Http_utils.redirect_to_with_actions
        "/participant/termsandconditions"
        [ Message.set ~error:[ "Please accept the terms and conditions." ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | Error (Pool_common.Error.NotFound Pool_common.Error.User)
    | Error (Pool_common.Error.NotFound Pool_common.Error.Participant) ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ "Invalid session, please login." ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | Error _ ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ "Invalid session!" ] ]
  in
  Rock.Middleware.create ~name:"participant.terms" ~filter
;;
