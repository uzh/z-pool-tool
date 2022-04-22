module Message = Http_utils.Message

let[@warning "-4"] confirmed_and_terms_agreed () =
  let filter handler req =
    let%lwt confirmed_and_terms_agreed =
      let open Utils.Lwt_result.Infix in
      let open Lwt_result.Syntax in
      let* context = Pool_context.find req |> Lwt_result.lift in
      let pool = context.Pool_context.tenant_db in
      let* user =
        Service.User.Web.user_from_session ~ctx:(Pool_tenant.to_ctx pool) req
        ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
      in
      let is_confirmed participant =
        Lwt_result.lift
          (match participant.Participant.user.Sihl_user.confirmed with
          | true -> Ok participant
          | false -> Error Pool_common.Message.ParticipantUnconfirmed)
      in
      let terms_agreed participant =
        let%lwt accepted = Participant.has_terms_accepted pool participant in
        match accepted with
        | true -> Lwt.return_ok participant
        | false ->
          Lwt.return_error Pool_common.Message.(TermsAndConditionsNotAccepted)
      in
      Pool_common.Id.of_string user.Sihl_user.id
      |> Participant.find pool
      |> Lwt_result.map_err
           (CCFun.const Pool_common.Message.(NotFound Field.Participant))
      >>= is_confirmed
      >>= terms_agreed
    in
    let query_lang =
      Pool_context.find req
      |> CCResult.to_opt
      |> CCFun.flip CCOption.bind (fun context ->
             context.Pool_context.query_language)
    in
    match confirmed_and_terms_agreed with
    | Ok _ -> handler req
    | Error Pool_common.Message.(NotFound Field.User)
    | Error Pool_common.Message.(NotFound Field.Participant) ->
      Http_utils.redirect_to_with_actions
        (Http_utils.path_with_language query_lang "/login")
        [ Message.set ~error:[ Pool_common.Message.SessionInvalid ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | Error Pool_common.Message.(TermsAndConditionsNotAccepted) ->
      Http_utils.redirect_to_with_actions
        (Http_utils.path_with_language query_lang "/termsandconditions")
        [ Message.set
            ~error:[ Pool_common.Message.(TermsAndConditionsNotAccepted) ]
        ]
    | Error Pool_common.Message.ParticipantUnconfirmed ->
      Http_utils.redirect_to
        (Http_utils.path_with_language query_lang "/email-confirmation")
    | _ ->
      Http_utils.redirect_to_with_actions
        (Http_utils.path_with_language query_lang "/login")
        [ Message.set ~error:[ Pool_common.Message.SessionInvalid ] ]
  in
  Rock.Middleware.create ~name:"participant.confirmed" ~filter
;;
