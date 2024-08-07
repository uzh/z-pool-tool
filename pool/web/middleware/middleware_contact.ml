module Message = Http_utils.Message

let[@warning "-4"] confirmed_and_terms_agreed () =
  let open Http_utils in
  let open Pool_message.Error in
  let filter handler req =
    let%lwt confirmed_and_terms_agreed =
      let open Utils.Lwt_result.Infix in
      let* { Pool_context.database_label; user; _ } =
        Pool_context.find req |> Lwt_result.lift
      in
      let* contact =
        let open Pool_context in
        let error = NotFound Pool_message.Field.User in
        user
        |> (function
              | Contact c -> Ok c
              | Admin _ | Guest -> Error error)
        |> Lwt_result.lift
      in
      let import_closed contact =
        let open Contact in
        if contact.import_pending |> Pool_user.ImportPending.value
        then Lwt_result.fail ImportPending
        else Lwt_result.return contact
      in
      let is_confirmed contact =
        Lwt_result.lift
          (match contact |> Contact.user |> Pool_user.is_confirmed with
           | true -> Ok contact
           | false -> Error ContactUnconfirmed)
      in
      let terms_agreed contact =
        let%lwt accepted = Contact.has_terms_accepted database_label contact in
        match accepted with
        | true -> Lwt.return_ok contact
        | false -> Lwt.return_error TermsAndConditionsNotAccepted
      in
      contact |> import_closed >>= is_confirmed >>= terms_agreed
    in
    let query_lang =
      Pool_context.find req
      |> CCResult.to_opt
      |> CCFun.flip CCOption.bind (fun { Pool_context.query_language; _ } ->
        query_language)
    in
    match confirmed_and_terms_agreed with
    | Ok _ -> handler req
    | Error (NotFound Pool_message.Field.User)
    | Error (NotFound Pool_message.Field.Contact) ->
      invalid_session_redirect req query_lang
    | Error TermsAndConditionsNotAccepted ->
      redirect_to_with_actions
        (path_with_language query_lang "/accept-terms?redirected=true")
        []
    | Error ContactUnconfirmed ->
      redirect_to (path_with_language query_lang "/email-confirmation")
    | Error ImportPending ->
      redirect_to_with_actions
        (path_with_language query_lang "/import-pending")
        [ Sihl.Web.Session.set [ "user_id", "" ] ]
    | _ -> invalid_session_redirect req query_lang
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
        [ Message.set ~error:[ Pool_message.Error.RequiredFieldsMissing ] ]
    | _ -> handler req
  in
  Rock.Middleware.create ~name:"contact.completion" ~filter
;;
