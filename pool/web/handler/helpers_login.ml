open CCFun.Infix
open Utils.Lwt_result.Infix
open Pool_message
module EmailAddress = Pool_user.EmailAddress
module Password = Pool_user.Password
module Response = Http_response
module HttpUtils = Http_utils

let src = Logs.Src.create "login helper"

let get_or_failwith_pool_error res =
  res
  |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
  |> CCResult.get_or_failwith
;;

let notify_user database_label tags email = function
  | None -> Lwt.return ()
  | Some (_ : Pool_user.FailedLoginAttempt.BlockedUntil.t) ->
    let notify () =
      Pool_user.find_active_by_email_opt database_label email
      >|> function
      | None -> Lwt_result.return ()
      | Some user ->
        let* tenant = Pool_tenant.find_by_label database_label in
        Message_template.AccountSuspensionNotification.create tenant user
        |>> Email.sent
            %> Pool_event.email
            %> Pool_event.handle_system_event ~tags database_label
        >|- fun err ->
        Logs.err (fun m ->
          m
            ~tags
            "Could not send account suspension notification to '%a': %s"
            EmailAddress.pp
            email
            Pool_common.(Utils.error_to_string Language.En err));
        err
    in
    () |> notify ||> CCFun.const ()
;;

let block_until counter =
  let open Pool_user.FailedLoginAttempt in
  let minutes =
    let counter = counter |> Counter.value in
    match counter with
    | _ when counter < 5 -> None
    | _ when counter < 6 -> Some 1
    | _ when counter < 7 -> Some 5
    | _ when counter < 8 -> Some 10
    | _ when counter < 9 -> Some 30
    | _ when counter < 10 -> Some 60
    | _ -> Some (60 * 24)
  in
  minutes
  |> CCOption.map (fun minutes ->
    let open Ptime in
    minutes * 60
    |> Span.of_int_s
    |> add_span (Pool_model.Time.now ())
    |> CCOption.get_exn_or "Invalid time span provided"
    |> BlockedUntil.create
    |> get_or_failwith_pool_error)
;;

let login_params urlencoded =
  let open Utils.Lwt_result.Infix in
  let urlencoded = Http_utils.remove_empty_values urlencoded in
  let* params =
    Field.[ Email; Password ]
    |> CCList.map Field.show
    |> Http_utils.urlencoded_to_params urlencoded
    |> CCOption.to_result Error.LoginProvideDetails
    |> Lwt_result.lift
  in
  let* email =
    CCList.assoc ~eq:String.equal Field.(Email |> show) params
    |> EmailAddress.create
    |> Lwt_result.lift
  in
  let password =
    CCList.assoc ~eq:String.equal Field.(Password |> show) params |> Password.Plain.create
  in
  Lwt_result.return (email, password)
;;

let log_request = Logging_helper.log_request_with_ip ~src

let validate_login req ~tags database_label ~email ~password =
  let open Pool_user.FailedLoginAttempt in
  let is_root = Database.Pool.is_root database_label in
  let handle_login login_attempts =
    let increment counter =
      let counter = Counter.increment counter in
      let blocked_until = block_until counter in
      let m =
        match login_attempts with
        | None -> create email counter blocked_until
        | Some login_attempts -> { login_attempts with counter; blocked_until }
      in
      let%lwt () = Repo.insert database_label m in
      Lwt.return m
    in
    let counter, blocked_until =
      match login_attempts with
      | Some { counter; blocked_until; _ } -> counter, blocked_until
      | None -> Counter.init, None
    in
    let suspension_error handler blocked_until =
      blocked_until
      |> CCOption.map BlockedUntil.value
      |> function
      | Some blocked when Ptime.(is_earlier (Pool_model.Time.now ()) ~than:blocked) ->
        Lwt_result.fail (Error.AccountTemporarilySuspended blocked)
      | None | _ -> handler ()
    in
    let handle_result = function
      | Ok user ->
        let%lwt () =
          login_attempts
          |> CCOption.map_or ~default:(Lwt.return ()) (Repo.delete database_label)
        in
        Lwt_result.return user
      | Error err ->
        log_request "Failed login attempt" req tags (Some email);
        let%lwt { blocked_until; _ } = counter |> increment in
        let%lwt () = notify_user database_label tags email blocked_until in
        suspension_error (fun () -> Lwt_result.fail err) blocked_until
    in
    let login () =
      let create_session () = Pool_user.login database_label email password in
      (match is_root with
       | true -> create_session ()
       | false ->
         User_import.find_pending_by_email_opt database_label email
         >|> (function
          | Some _ -> Lwt.return_error Pool_message.(Error.Invalid Field.Password)
          | None -> create_session ()))
      >|> handle_result
    in
    suspension_error login blocked_until
  in
  email |> Pool_user.FailedLoginAttempt.Repo.find_opt database_label >|> handle_login
;;

(* Internal helper: create 2FA auth token and email job events for a validated user *)
let create_2fa_auth ?id ?token ~tags req { Pool_context.database_label; language; _ } user
  =
  let auth = Authentication.(create ?id ?token ~user ~channel:Channel.Email) () in
  let%lwt email_job =
    let open Message_template in
    let email_layout =
      match Database.Pool.is_root database_label with
      | true -> Root
      | false -> Tenant (Pool_context.Tenant.get_tenant_exn req)
    in
    Login2FAToken.prepare database_label language email_layout
  in
  let* events =
    Cqrs_command.Login_command.Create2FaLogin.handle ~tags ~email_job user auth
    |> Lwt_result.lift
  in
  Lwt_result.return (user, auth, events)
;;

let create_2fa_login
      ?id
      ?token
      ?tags
      req
      ({ Pool_context.database_label; _ } as context)
      urlencoded
  =
  let tags = CCOption.value tags ~default:(Pool_context.Logger.Tags.req req) in
  let* email, password = login_params urlencoded in
  let* user = validate_login req ~tags database_label ~email ~password in
  create_2fa_auth ?id ?token ~tags req context user
;;

let admin_has_smtp_permission database_label (user : Pool_user.t) =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx database_label in
  let%lwt result =
    user.Pool_user.id
    |> Admin.(Id.of_user %> find database_label)
    >>= Admin.Guard.Actor.to_authorizable ~ctx
    >>= Guard.Persistence.validate database_label Cqrs_command.Smtp_command.Create.effects
  in
  Lwt.return (CCResult.is_ok result)
;;

type login_step =
  | MfaRequired of Pool_user.t * Authentication.t * Pool_event.t list
  | DirectLogin of Pool_user.t

(** Entry point for the login POST handler. Determines whether 2FA is needed based
    on the SMTP configuration and the user type/permissions. *)
let initiate_login
      ?id
      ?token
      ?tags
      req
      ({ Pool_context.database_label; _ } as context)
      urlencoded
  =
  let tags = CCOption.value tags ~default:(Pool_context.Logger.Tags.req req) in
  let* email, password = login_params urlencoded in
  let* user = validate_login req ~tags database_label ~email ~password in
  let%lwt smtp_set = Email.SmtpAuth.defalut_is_set database_label in
  match Database.Pool.is_root database_label with
  | true ->
    (* Root user: always go through MFA, may have to check the DB manually *)
    let* login_user, auth, events = create_2fa_auth ?id ?token ~tags req context user in
    Lwt_result.return (MfaRequired (login_user, auth, events))
  | false when smtp_set ->
    (* SMTP configured: all users go through MFA *)
    let* login_user, auth, events = create_2fa_auth ?id ?token ~tags req context user in
    Lwt_result.return (MfaRequired (login_user, auth, events))
  | false ->
    (* No SMTP: user type determines behaviour *)
    (match%lwt[@warning "-4"] Contact.find_by_user database_label user with
     | Ok (_ : Contact.t) -> Lwt_result.fail Pool_message.Error.ContactLoginDisabled
     | Error (Pool_message.Error.NotFound Field.Contact) ->
       let%lwt has_perm = admin_has_smtp_permission database_label user in
       if has_perm
       then Lwt_result.return (DirectLogin user)
       else Lwt_result.fail Pool_message.Error.AdminLoginDisabled
     | Error err -> Lwt_result.fail err)
;;

let decode_2fa_confirmation database_label req ~tags =
  let log_request = log_request "Failed to confirm 2FA login" req tags in
  let open Cqrs_command.Login_command.Confirm2FaLogin in
  let* auth_id, token =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    decode urlencoded
    |> Lwt_result.lift
    >|- fun err ->
    log_request None;
    err
  in
  let* authentication, user =
    Authentication.find_valid_by_id database_label auth_id
    >|- fun err ->
    log_request None;
    err
  in
  Lwt_result.return (user, authentication, token)
;;

let confirm_2fa_login ~tags user authentication token req =
  let log_request = log_request "Failed to confirm 2FA login" req tags in
  let open Cqrs_command.Login_command.Confirm2FaLogin in
  let* events =
    let open Lwt_result in
    handle ~tags authentication token
    |> lift
    >|- fun err ->
    log_request (Some (Pool_user.email user));
    err
  in
  Lwt_result.return (user, events)
;;

(* Register a failed login attempt for [email], increasing the block duration and
   notifying the user if the account gets (temporarily) suspended. Mirrors the
   counter handling in [validate_login]. *)
let increment_failed_login_attempt ~tags database_label email =
  let open Pool_user.FailedLoginAttempt in
  let%lwt login_attempts = Repo.find_opt database_label email in
  let counter =
    match login_attempts with
    | Some { counter; _ } -> counter
    | None -> Counter.init
  in
  let counter = Counter.increment counter in
  let blocked_until = block_until counter in
  let m =
    match login_attempts with
    | None -> create email counter blocked_until
    | Some login_attempts -> { login_attempts with counter; blocked_until }
  in
  let%lwt () = Repo.insert database_label m in
  notify_user database_label tags email blocked_until
;;

type verify_outcome =
  | Verified of Pool_user.t
  | InvalidToken of Pool_message.Error.t
  | SessionExpired of Authentication.Id.t
  | SessionMissing

let verify_2fa_login ~tags { Pool_context.database_label; user = context_user; _ } req =
  match Sihl.Web.Session.find "auth_id" req with
  | None -> Lwt.return SessionMissing
  | Some id ->
    let auth_id = Authentication.Id.of_string id in
    auth_id
    |> Authentication.find_valid_by_id database_label
    >|> (function
     | Error _ -> Lwt.return (SessionExpired auth_id)
     | Ok (auth, user) ->
       let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
       (match Cqrs_command.Login_command.Confirm2FaLogin.decode urlencoded with
        | Error err -> Lwt.return (InvalidToken err)
        | Ok (_, token) ->
          confirm_2fa_login ~tags user auth token req
          >|> (function
           | Ok (user, events) ->
             let%lwt () = Pool_event.handle_events database_label context_user events in
             Lwt.return (Verified user)
           | Error err ->
             let%lwt () =
               Pool_event.handle_events
                 database_label
                 context_user
                 [ Authentication.IncreaseUsageCount auth |> Pool_event.authentication ]
             in
             let remaining =
               Authentication.UsageCount.(value limit)
               - Authentication.UsageCount.value auth.Authentication.usage_count
               - 1
             in
             if remaining <= 0
             then (
               let%lwt () =
                 increment_failed_login_attempt
                   ~tags
                   database_label
                   (Pool_user.email user)
               in
               Lwt.return (SessionExpired auth_id))
             else Lwt.return (InvalidToken err))))
;;

let login_verify_get ~render_confirmation ~login_path req =
  let open HttpUtils in
  let tags = Pool_context.Logger.Tags.req req in
  let handle_request (Pool_context.{ database_label; _ } as context) =
    let%lwt auth_data =
      match Sihl.Web.Session.find "auth_id" req with
      | Some auth_id ->
        Authentication.Id.of_string auth_id
        |> Authentication.find_valid_by_id database_label
        >|- fun _ ->
        let _ =
          Pool_common.Utils.with_log_error
            ~tags
            (Pool_message.Error.Authorization
               (Format.asprintf
                  "Failed to authenticate user: no valid authentication found for id %s"
                  auth_id))
        in
        ()
      | None -> Lwt_result.fail ()
    in
    match auth_data with
    | Ok (auth, user) -> render_confirmation context auth user
    | Error () ->
      Http_utils.redirect_to_with_actions
        login_path
        [ Sihl.Web.Session.update_or_set_value ~key:"auth_id" (fun _ -> None) req
        ; Message.set
            ~warning:
              [ Pool_message.Warning.Warning
                  "Your session has expired, please sign in again"
              ]
        ]
      |> Lwt_result.ok
  in
  Http_response.handle ~src req handle_request
;;

let login_verify_post ~verify_path ~handle_verified ~handle_invalid_session req =
  let open HttpUtils in
  let tags = Pool_context.Logger.Tags.req req in
  let handle_request (Pool_context.{ database_label; _ } as context) =
    match%lwt verify_2fa_login ~tags context req with
    | Verified user -> handle_verified context user
    | InvalidToken _ ->
      HttpUtils.redirect_to_with_actions
        verify_path
        [ Message.set ~error:[ Pool_message.(Error.Invalid Field.OTP) ] ]
      |> Lwt_result.ok
    | SessionExpired auth_id ->
      let%lwt () =
        Pool_event.handle_event
          ~tags
          database_label
          context.Pool_context.user
          Pool_event.(Authentication (Authentication.Deleted auth_id))
      in
      handle_invalid_session ()
      >|+ Sihl.Web.Session.update_or_set_value ~key:"auth_id" (fun _ -> None) req
    | SessionMissing -> handle_invalid_session ()
  in
  Response.handle ~src req handle_request
;;

let resend_token_post ~verify_path req =
  let open HttpUtils in
  let tags = Pool_context.Logger.Tags.req req in
  let cooldown_seconds = 60 in
  let handle_request (Pool_context.{ database_label; user; _ } as context) =
    let%lwt result =
      let* auth_id =
        match Sihl.Web.Session.find "auth_id" req with
        | None -> Lwt.return_error Pool_message.(Error.Invalid Field.Id)
        | Some auth_id -> Authentication.Id.of_string auth_id |> Lwt.return_ok
      in
      let* (_ : Authentication.t), login_user =
        Authentication.find_valid_by_id database_label auth_id
      in
      let* () =
        let%lwt last_sent_at =
          Pool_queue.find_last_login_token_sent_at
            database_label
            (Pool_common.Id.of_string (Authentication.Id.value auth_id))
        in
        (match last_sent_at with
         | None -> Ok ()
         | Some sent_at ->
           let elapsed =
             Ptime.diff (Ptime_clock.now ()) sent_at
             |> Ptime.Span.to_float_s
             |> int_of_float
           in
           if elapsed < Authentication.resend_cooldown_seconds
           then Error Pool_message.Error.TokenAlreadySentRecently
           else Ok ())
        |> Lwt_result.lift
      in
      let* _, (_ : Authentication.t), events =
        create_2fa_auth ~id:auth_id ~tags req context login_user
      in
      let%lwt () = Pool_event.handle_events database_label user events in
      Lwt.return_ok ()
    in
    Http_response.Htmx.redirect
      verify_path
      ~actions:
        (match result with
         | Ok _ -> [ Message.set ~success:[ Pool_message.(Success.Resent Field.OTP) ] ]
         | Error e -> [ Message.set ~error:[ e ] ])
    |> Lwt_result.ok
  in
  Response.handle ~src req handle_request
;;
