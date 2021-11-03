module Message = Http_utils.Message

let confirmed () =
  let filter handler req =
    let%lwt is_confirmed =
      let open Lwt_result.Syntax in
      let* user_id =
        req
        |> Sihl.Web.Session.find "user_id"
        |> CCOpt.map Pool_common.Id.of_string
        |> CCOpt.to_result (`UserNotFound, "User not found!")
        |> Lwt_result.lift
      in
      let* pool =
        Middleware_tenant.tenant_db_of_request req
        |> Lwt_result.map_err (fun msg -> `TenantNotFound, msg)
      in
      Lwt_result.map
        (fun participant -> participant.Participant.user.Sihl_user.confirmed)
        (Participant.find pool user_id
        |> Lwt_result.map_err (fun msg -> `ParticipantError, msg))
    in
    match is_confirmed with
    | Ok _ -> handler req
    | Error (label, _) ->
      (match label with
      | `UserNotFound | `ParticipantNotFound ->
        Http_utils.redirect_to_with_actions
          "/login"
          [ Message.set ~error:[ "Invalid session, please login." ]
          ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
          ]
      | _ -> Http_utils.redirect_to "/participant/email-confirmation")
  in
  Rock.Middleware.create ~name:"participant.confirmed" ~filter
;;
