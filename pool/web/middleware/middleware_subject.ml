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
      let is_confirmed subject =
        Lwt_result.lift
          (match subject.Subject.user.Sihl_user.confirmed with
          | true -> Ok subject
          | false -> Error Pool_common.Message.SubjectUnconfirmed)
      in
      let terms_agreed subject =
        let%lwt accepted = Subject.has_terms_accepted pool subject in
        match accepted with
        | true -> Lwt.return_ok subject
        | false ->
          Lwt.return_error Pool_common.Message.(TermsAndConditionsNotAccepted)
      in
      Pool_common.Id.of_string user.Sihl_user.id
      |> Subject.find pool
      |> Lwt_result.map_err
           (CCFun.const Pool_common.Message.(NotFound Field.Subject))
      >>= is_confirmed
      >>= terms_agreed
    in
    match confirmed_and_terms_agreed with
    | Ok _ -> handler req
    | Error Pool_common.Message.(NotFound Field.User)
    | Error Pool_common.Message.(NotFound Field.Subject) ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ Pool_common.Message.SessionInvalid ]
        ; Sihl.Web.Flash.set [ "_redirect_to", req.Rock.Request.target ]
        ]
    | Error Pool_common.Message.(TermsAndConditionsNotAccepted) ->
      Http_utils.redirect_to_with_actions
        "/termsandconditions"
        [ Message.set
            ~error:[ Pool_common.Message.(TermsAndConditionsNotAccepted) ]
        ]
    | Error Pool_common.Message.SubjectUnconfirmed ->
      Http_utils.redirect_to "/email-confirmation"
    | _ ->
      Http_utils.redirect_to_with_actions
        "/login"
        [ Message.set ~error:[ Pool_common.Message.SessionInvalid ] ]
  in
  Rock.Middleware.create ~name:"subject.confirmed" ~filter
;;
