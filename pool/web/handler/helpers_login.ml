module Label = Pool_database.Label
module Message = Pool_common.Message

let src = Logs.Src.create "login helper"

let get_or_failwith_pool_error res =
  res
  |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
  |> CCResult.get_or_failwith
;;

let notify_user database_label tags email =
  let open Utils.Lwt_result.Infix in
  function
  | None -> Lwt.return ()
  | Some (_ : Pool_user.FailedLoginAttempt.BlockedUntil.t) ->
    let notify () =
      email
      |> Service.User.find_by_email_opt
           ~ctx:(Pool_database.to_ctx database_label)
      >|> function
      | None -> Lwt_result.return ()
      | Some user ->
        let* tenant = Pool_tenant.find_by_label database_label in
        Message_template.AccountSuspensionNotification.create tenant user
        |>> (fun message ->
              Email.Sent (message, None)
              |> Pool_event.email
              |> Pool_event.handle_event ~tags database_label)
        >|- fun err ->
        Logs.err (fun m ->
          m
            ~tags
            "Could not send account suspension notification to '%s': %s"
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
       |> add_span (Ptime_clock.now ())
       |> CCOption.get_exn_or "Invalid time span provided"
       |> BlockedUntil.create
       |> get_or_failwith_pool_error)
;;

let login_params urlencoded =
  let open Utils.Lwt_result.Infix in
  let open Message in
  let* params =
    Field.[ Email; Password ]
    |> CCList.map Field.show
    |> Http_utils.urlencoded_to_params urlencoded
    |> CCOption.to_result LoginProvideDetails
    |> Lwt_result.lift
  in
  let email = CCList.assoc ~eq:String.equal Field.(Email |> show) params in
  let password =
    CCList.assoc ~eq:String.equal Field.(Password |> show) params
  in
  Lwt_result.return (email, password)
;;

let log_request req tags email =
  let open Opium in
  let open Request in
  let ip =
    Headers.get req.headers "X-Real-IP"
    |> CCOption.value ~default:"X-Real-IP not found"
  in
  Logs.info ~src (fun m -> m "Failed login attempt: %s %s" ip email ~tags)
;;

let login req urlencoded database_label =
  let open Utils.Lwt_result.Infix in
  let open Pool_user.FailedLoginAttempt in
  let tags = Pool_context.Logger.Tags.req req in
  let* email, password = login_params urlencoded in
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
      | Some blocked when Ptime.(is_earlier (Ptime_clock.now ()) ~than:blocked)
        -> Lwt_result.fail (Message.AccountTemporarilySuspended blocked)
      | None | _ -> handler ()
    in
    let handle_result = function
      | Ok user ->
        let%lwt () =
          login_attempts
          |> CCOption.map_or
               ~default:(Lwt.return ())
               (Repo.delete database_label)
        in
        Lwt_result.return user
      | Error err ->
        log_request req tags email;
        let%lwt { blocked_until; _ } = counter |> increment in
        let%lwt () = notify_user database_label tags email blocked_until in
        suspension_error
          (fun () -> err |> Message.handle_sihl_login_error |> Lwt_result.fail)
          blocked_until
    in
    let login () =
      Service.User.login
        ~ctx:(Pool_database.to_ctx database_label)
        email
        ~password
      >|> handle_result
    in
    suspension_error login blocked_until
  in
  email
  |> Pool_user.FailedLoginAttempt.Repo.find_opt database_label
  >|> handle_login
;;
