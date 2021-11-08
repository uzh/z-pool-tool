module Message = Http_utils.Message

let confirmed () =
  let filter handler req =
    let%lwt confirmed_and_terms_agreed =
      let open Utils.Lwt_result.Infix in
      let open Lwt_result.Syntax in
      let* pool =
        Middleware_tenant.tenant_db_of_request req
        |> Lwt_result.map_err (fun msg -> `TenantNotFound, msg)
      in
      let* user =
        Service.User.Web.user_from_session
          ~ctx:[ "pool", pool |> Pool_common.Database.Label.value ]
          req
        |> Lwt.map (CCOpt.to_result (`UserNotFound, "User not found!"))
        >>= fun user ->
        match user.Sihl.Contract.User.admin with
        | true -> Lwt.return_error (`UserIsAdmin, "User is admin")
        | false -> Lwt.return_ok user
      in
      Lwt_result.bind_result
        (Participant.find pool (Pool_common.Id.of_string user.Sihl_user.id)
        |> Lwt_result.map_err (fun msg -> `ParticipantError, msg))
        (fun participant ->
          if participant.Participant.user.Sihl_user.confirmed
          then Ok participant
          else Error (`ParticipantUnconfirmed, "Participant isn't confirmed!"))
    in
    match confirmed_and_terms_agreed with
    | Ok _ -> handler req
    | Error (label, _) ->
      (match label with
      | `UserNotFound | `ParticipantNotFound ->
        Http_utils.redirect_to_with_actions
          "/login"
          [ Message.set ~error:[ "Invalid session, please login." ]
          ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
          ]
      | `UserIsAdmin ->
        Http_utils.redirect_to_with_actions "/admin/dashboard" []
      | _ -> Http_utils.redirect_to "/participant/email-confirmation")
  in
  Rock.Middleware.create ~name:"participant.confirmed" ~filter
;;
