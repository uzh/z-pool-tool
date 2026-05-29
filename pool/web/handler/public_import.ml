open Utils.Lwt_result.Infix
open Pool_message
module Response = Http_response

let src = Logs.Src.create "handler.public.import"

let import_pending req =
  let open Pool_common.I18n in
  General.note ~title:ImportPendingTitle ~body:ImportPendingNote req
;;

let user_and_import_from_token database_label token =
  let open Pool_context in
  token
  |> User_import.find_pending_by_token database_label
  >>= fun ({ User_import.user_uuid; _ } as import) ->
  user_uuid
  |> Pool_user.find_exn database_label
  >|> context_user_of_user database_label
  ||> fun user ->
  match user with
  | Guest -> Error (Error.Invalid Field.Token)
  | Admin _ | Contact _ -> Ok (import, user)
;;

let user_import_from_req database_label req =
  let open Utils.Lwt_result.Infix in
  Sihl.Web.Request.query Field.(show Token) req
  |> CCOption.to_result (Error.NotFound Field.Token)
  |> Lwt_result.lift
  >== User_import.Token.create
  >>= user_and_import_from_token database_label
;;

let render_import_confirmation_page req context user_import user =
  let open Pool_context in
  let { Pool_context.database_label; language; _ } = context in
  let { User_import.token; active_after_import; _ } = user_import in
  let email =
    match user with
    | Admin admin -> Admin.email_address admin |> Pool_user.EmailAddress.value
    | Contact contact -> Contact.email_address contact |> Pool_user.EmailAddress.value
    | Guest -> ""
  in
  Response.bad_request_render_error context
  @@
  let%lwt password_policy =
    I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
  in
  let%lwt terms = I18n.find_by_key database_label I18n.Key.TermsAndConditions language in
  Page.Public.Import.import_confirmation
    ~active_after_import:(User_import.ActiveAfterImport.value active_after_import)
    ~email
    context
    token
    password_policy
    terms
  |> General.create_tenant_layout req context
  >|+ Sihl.Web.Response.of_html
;;

let import_confirmation req =
  let result ({ Pool_context.database_label; _ } as context) =
    let* user_import, user =
      user_import_from_req database_label req >|- Response.not_found
    in
    render_import_confirmation_page req context user_import user
  in
  Response.handle ~src req result
;;

let bad_request_confirmation (res : ('a, Error.t) Lwt_result.t) =
  Response.bad_request_on_error import_confirmation res
;;

let decode_confirm_import_cmd urlencoded user_import user =
  let open CCResult in
  let open Cqrs_command.User_import_command.ConfirmImport in
  urlencoded |> decode >>= handle (user_import, user) |> Lwt_result.lift
;;

let decode_activate_import_cmd user_import user =
  Cqrs_command.User_import_command.ActivateImport.handle (user_import, user)
  |> Lwt_result.lift
;;

let import_confirmation_events urlencoded user_import user =
  if User_import.ActiveAfterImport.value user_import.User_import.active_after_import
  then decode_confirm_import_cmd urlencoded user_import user
  else decode_activate_import_cmd user_import user
;;

let target_user_message_language default_language = function
  | Pool_context.Admin _ -> default_language
  | Pool_context.Contact contact ->
    CCOption.value ~default:default_language contact.Contact.language
  | Pool_context.Guest -> default_language
;;

let target_user_pool_user = function
  | Pool_context.Admin admin -> Ok admin.Admin.user
  | Pool_context.Contact contact -> Ok contact.Contact.user
  | Pool_context.Guest -> Error Pool_message.(Error.Invalid Field.User)
;;

let password_reset_events_for_target_user req database_label language tags target_user =
  let open Utils.Lwt_result.Infix in
  let tenant = Pool_context.Tenant.get_tenant_exn req in
  let message_language = target_user_message_language language target_user in
  let* user = target_user_pool_user target_user |> Lwt_result.lift in
  let* reset_message =
    Message_template.PasswordReset.create
      database_label
      message_language
      (Message_template.Tenant tenant)
      user
  in
  Cqrs_command.Common_command.ResetPassword.handle ~tags reset_message |> Lwt_result.lift
;;

let import_confirmation_post req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    let open Http_utils in
    Sihl.Web.Request.to_urlencoded req
    ||> remove_empty_values
    ||> format_request_boolean_values Field.[ show TermsAccepted ]
  in
  let result
        { Pool_context.database_label; query_parameters; user = actor_user; language; _ }
    =
    let* user_import, target_user =
      let* token =
        urlencoded
        |> Http_utils.find_in_urlencoded Field.Token
        |> Lwt_result.lift
        >== User_import.Token.create
        |> bad_request_confirmation
      in
      user_and_import_from_token database_label token >|- Response.not_found
    in
    let* () =
      Helpers.terms_and_conditions_accepted urlencoded |> bad_request_confirmation
    in
    let* events =
      import_confirmation_events urlencoded user_import target_user
      |> bad_request_confirmation
    in
    let* all_events =
      if User_import.ActiveAfterImport.value user_import.User_import.active_after_import
      then Lwt_result.return events
      else
        let* reset_events =
          password_reset_events_for_target_user
            req
            database_label
            language
            tags
            target_user
          |> bad_request_confirmation
        in
        Lwt_result.return (events @ reset_events)
    in
    (* Public token flow: command target user may differ from unauthenticated actor user. *)
    let%lwt () = Pool_event.handle_events ~tags database_label actor_user all_events in
    let success_message =
      if User_import.ActiveAfterImport.value user_import.User_import.active_after_import
      then Success.ImportCompleted
      else Success.ImportActivationPasswordResetSent
    in
    Http_utils.(
      redirect_to_with_actions
        (url_with_field_params query_parameters "/login")
        [ Message.set ~success:[ success_message ] ])
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let contact_import_from_req { Pool_context.database_label; user; _ } req =
  let open Utils.Lwt_result.Infix in
  let contact_from_unsubscribe_token token =
    let%lwt user_id_opt = Pool_token.read database_label token ~k:"user_id" in
    let%lwt token_type_opt = Pool_token.read database_label token ~k:"type" in
    match token_type_opt, user_id_opt with
    | Some "unsubscribe", Some user_id ->
      Contact.find database_label (Contact.Id.of_string user_id)
      ||> CCResult.map (fun contact -> Some token, contact)
    | _ -> Lwt_result.fail Pool_message.(Error.NotFound Field.Token)
  in
  let contact_from_context_user () =
    match user with
    | Pool_context.Contact contact -> Lwt.return_ok (None, contact)
    | Pool_context.Admin _ | Pool_context.Guest ->
      Lwt.return_error Pool_message.(Error.NotFound Field.Contact)
  in
  match Sihl.Web.Request.query Field.(show Token) req with
  | Some token -> Pool_token.of_string token |> contact_from_unsubscribe_token
  | None -> contact_from_context_user ()
;;

let unsubscribe req =
  let open Utils.Lwt_result.Infix in
  let result context =
    contact_import_from_req context req
    >|> function
    | Error (_ : Error.t) -> Lwt_result.fail Response.generic_not_found
    | Ok (token_opt, contact) ->
      let email = contact |> Contact.email_address |> Pool_user.EmailAddress.value in
      Page.Contact.pause_account context ?token:token_opt ~email ()
      |> General.create_tenant_layout req context
      >|+ Sihl.Web.Response.of_html
      |> Response.bad_request_render_error context
  in
  Response.handle ~src req result
;;

let unsubscribe_post req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let result ({ Pool_context.database_label; query_parameters; user; _ } as context) =
    contact_import_from_req context req
    >|> function
    | Error (_ : Error.t) -> Lwt_result.fail Response.generic_not_found
    | Ok (token, contact) ->
      let paused = Pool_user.Paused.(create true) in
      Cqrs_command.Contact_command.TogglePaused.handle ~tags contact paused
      |> Lwt_result.lift
      |> Response.bad_request_on_error unsubscribe
      |>> fun events ->
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      let%lwt () =
        CCOption.map_or
          ~default:Lwt.return_unit
          Pool_token.(deactivate database_label)
          token
      in
      Http_utils.(
        redirect_to_with_actions
          (url_with_field_params query_parameters "/index")
          [ Message.set ~success:[ Success.PausedToggled true ] ])
  in
  Response.handle ~src req result
;;
