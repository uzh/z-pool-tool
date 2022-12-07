module Message = Http_utils.Message

let[@warning "-4"] confirmed_and_terms_agreed () =
  let filter handler req =
    let%lwt confirmed_and_terms_agreed =
      let open Utils.Lwt_result.Infix in
      let* { Pool_context.tenant_db; user; _ } =
        Pool_context.find req |> Lwt_result.lift
      in
      let* contact =
        let open Pool_context in
        let error = Pool_common.Message.(NotFound Field.User) in
        user
        |> CCOption.map_or ~default:(Error error) (function
             | Contact c -> Ok c
             | Admin _ | Root _ -> Error error)
        |> Lwt_result.lift
      in
      let is_confirmed contact =
        Lwt_result.lift
          (match contact.Contact.user.Sihl_user.confirmed with
           | true -> Ok contact
           | false -> Error Pool_common.Message.ContactUnconfirmed)
      in
      let terms_agreed contact =
        let%lwt accepted = Contact.has_terms_accepted tenant_db contact in
        match accepted with
        | true -> Lwt.return_ok contact
        | false ->
          Lwt.return_error Pool_common.Message.(TermsAndConditionsNotAccepted)
      in
      contact |> is_confirmed >>= terms_agreed
    in
    let query_lang =
      Pool_context.find req
      |> CCResult.to_opt
      |> CCFun.flip CCOption.bind (fun { Pool_context.query_language; _ } ->
           query_language)
    in
    match confirmed_and_terms_agreed with
    | Ok _ -> handler req
    | Error Pool_common.Message.(NotFound Field.User)
    | Error Pool_common.Message.(NotFound Field.Contact) ->
      Http_utils.invalid_session_redirect req query_lang
    | Error Pool_common.Message.(TermsAndConditionsNotAccepted) ->
      Http_utils.redirect_to_with_actions
        (Http_utils.path_with_language query_lang "/termsandconditions")
        [ Message.set
            ~error:[ Pool_common.Message.(TermsAndConditionsNotAccepted) ]
        ]
    | Error Pool_common.Message.ContactUnconfirmed ->
      Http_utils.redirect_to
        (Http_utils.path_with_language query_lang "/email-confirmation")
    | _ -> Http_utils.invalid_session_redirect req query_lang
  in
  Rock.Middleware.create ~name:"contact.confirmed" ~filter
;;

let completion_in_progress () =
  let filter handler req =
    let query_lang =
      Pool_context.find req
      |> CCResult.to_opt
      |> CCFun.flip CCOption.bind (fun { Pool_context.query_language; _ } ->
           query_language)
    in
    match Sihl.Web.Session.find Contact.profile_completion_cookie req with
    | Some "true" ->
      Http_utils.redirect_to_with_actions
        (Http_utils.path_with_language query_lang "/user/completion")
        [ Message.set ~error:[ Pool_common.Message.(RequiredFieldsMissing) ] ]
    | _ -> handler req
  in
  Rock.Middleware.create ~name:"contact.completion" ~filter
;;
