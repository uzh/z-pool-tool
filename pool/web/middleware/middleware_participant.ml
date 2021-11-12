module Message = Http_utils.Message

let[@warning "-4"] confirmed () =
  let filter handler req =
    let%lwt confirmed_and_terms_agreed =
      let open Utils.Lwt_result.Infix in
      let open Lwt_result.Syntax in
      let* pool = Middleware_tenant.tenant_db_of_request req in
      let* user_id =
        Service.User.Web.user_from_session
          ~ctx:(Pool_common.Utils.pool_to_ctx pool)
          req
        ||> CCOption.to_result Pool_common.Message.(NotFound User)
        >|= fun user -> Pool_common.Id.of_string user.Sihl_user.id
      in
      Participant.find pool user_id
      >>= fun participant ->
      if participant.Participant.user.Sihl_user.confirmed
      then Lwt.return_ok participant
      else Lwt.return_error Pool_common.Message.ParticipantUnconfirmed
    in
    match confirmed_and_terms_agreed with
    | Ok _ -> handler req
    | Error Pool_common.Message.(NotFound User)
    | Error Pool_common.Message.(NotFound Participant) ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ Pool_common.Message.SessionInvalid ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | _ -> Http_utils.redirect_to "/participant/email-confirmation"
  in
  Rock.Middleware.create ~name:"participant.confirmed" ~filter
;;
