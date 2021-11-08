module Message = Http_utils.Message
module Common = Pool_common

let[@warning "-4"] terms_accepted () =
  let filter handler req =
    let%lwt has_accepted =
      let open Lwt_result.Syntax in
      let* user_id =
        req
        |> Sihl.Web.Session.find "user_id"
        |> CCOption.map Common.Id.of_string
        |> CCOption.to_result Common.Message.(NotFound User)
        |> Lwt_result.lift
      in
      let* pool = Middleware_tenant.tenant_db_of_request req in
      let* participant = Participant.find pool user_id in
      Participant.has_terms_accepted pool participant |> Lwt_result.ok
    in
    match has_accepted with
    | Ok true -> handler req
    | Ok false ->
      Http_utils.redirect_to_with_actions
        "/participant/termsandconditions"
        [ Message.set ~error:[ Common.Message.TermsAndConditionsNotAccepted ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | Error Common.Message.(NotFound User)
    | Error Common.Message.(NotFound Participant) ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ Common.Message.SessionInvalid ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | Error _ ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ Common.Message.SessionInvalid ] ]
  in
  Rock.Middleware.create ~name:"participant.terms" ~filter
;;
