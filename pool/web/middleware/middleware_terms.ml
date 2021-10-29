module Message = Http_utils.Message

let terms_accepted () =
  let filter handler req =
    let%lwt has_accepted =
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
      let* participant =
        Participant.find pool user_id
        |> Lwt_result.map_err (fun msg -> `ParticipantNotFound, msg)
      in
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
    | Error (label, _) ->
      Http_utils.redirect_to_with_actions
        "/login"
        (match label with
        | `UserNotFound | `ParticipantNotFound ->
          [ Message.set ~error:[ "Invalid session, please login." ]
          ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
          ]
        | _ -> [ Message.set ~error:[ "Invalid session!" ] ])
  in
  Rock.Middleware.create ~name:"auth_payout" ~filter
;;
