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

let import_confirmation req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    let* { User_import.token; _ }, _ =
      user_import_from_req database_label req >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let%lwt password_policy =
      I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
    in
    let%lwt terms =
      I18n.find_by_key database_label I18n.Key.TermsAndConditions language
    in
    Page.Public.Import.import_confirmation context token password_policy terms
    |> General.create_tenant_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let bad_request_confirmation (res : ('a, Error.t) Lwt_result.t) =
  Response.bad_request_on_error import_confirmation res
;;

(* TODO: Test *)
let import_confirmation_post req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    let open Http_utils in
    Sihl.Web.Request.to_urlencoded req
    ||> remove_empty_values
    ||> format_request_boolean_values Field.[ show TermsAccepted ]
  in
  let result { Pool_context.database_label; query_parameters; _ } =
    let* user_import, user =
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
      let open CCResult in
      let open Cqrs_command.User_import_command.ConfirmImport in
      urlencoded
      |> decode
      >>= handle (user_import, user)
      |> Lwt_result.lift
      |> bad_request_confirmation
    in
    (* TODO: This takes the imported user as argument, but there is no logged in user *)
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.(
      redirect_to_with_actions
        (url_with_field_params query_parameters "/login")
        [ Message.set ~success:[ Success.ImportCompleted ] ])
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let contact_import_from_req { Pool_context.database_label; _ } req =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  user_import_from_req database_label req
  ||> function
  | Error err -> Error err
  | Ok (_, Admin _) | Ok (_, Guest) -> Error Pool_message.(Error.NotFound Field.Contact)
  | Ok ({ User_import.token; _ }, Contact contact) -> Ok (token, contact)
;;

let unsubscribe req =
  let open Utils.Lwt_result.Infix in
  let result context =
    contact_import_from_req context req
    >|> function
    | Error (_ : Error.t) -> Lwt_result.fail Response.generic_not_found
    | Ok (token, _) ->
      Page.Contact.pause_account context ~token ()
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
    | Ok (_, contact) ->
      let paused = Pool_user.Paused.(create true) in
      Cqrs_command.Contact_command.TogglePaused.handle ~tags contact paused
      |> Lwt_result.lift
      |> Response.bad_request_on_error unsubscribe
      |>> fun events ->
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.(
        redirect_to_with_actions
          (url_with_field_params query_parameters "/index")
          [ Message.set ~success:[ Success.PausedToggled true ] ])
  in
  Response.handle ~src req result
;;
