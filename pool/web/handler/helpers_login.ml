module Label = Pool_database.Label
module Message = Pool_common.Message

let src = Logs.Src.create "login helper"

module Cache = struct
  open Hashtbl

  let tbl : (Label.t * string, int * Ptime.t option) t = create 100
  let set key value = replace tbl key value
  let remove = remove tbl
  let find_opt = find_opt tbl
end

let notify_user database_label (counter, _) tags email =
  let open Utils.Lwt_result.Infix in
  if not (CCInt.equal counter 5)
  then Lwt.return ()
  else (
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
              Email.Sent message
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
    () |> notify ||> CCFun.const ())
;;

let block_until counter =
  let minutes =
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
  |> CCOption.map (fun min ->
       min * 60
       |> Ptime.Span.of_int_s
       |> Ptime.add_span (Ptime_clock.now ())
       |> CCOption.get_exn_or "Invalid time span provided")
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
  let tags = Pool_context.Logger.Tags.req req in
  let* email, password = login_params urlencoded in
  let init = 0, None in
  let increment counter =
    let counter = counter + 1 in
    counter, block_until counter
  in
  let key = database_label, email in
  let handle_login (counter, blocked) =
    let suspension_error handler = function
      | Some blocked when Ptime.(is_earlier (Ptime_clock.now ()) ~than:blocked)
        -> Lwt_result.fail (Message.AccountTemporarilySuspended blocked)
      | None | _ -> handler ()
    in
    let handle_result = function
      | Ok user ->
        let () = Cache.remove key in
        Lwt_result.return user
      | Error err ->
        log_request req tags email;
        let ((_, blocked) as counter) = counter |> increment in
        let () = Cache.set key counter in
        let%lwt () = notify_user database_label counter tags email in
        suspension_error
          (fun () -> err |> Message.handle_sihl_login_error |> Lwt_result.fail)
          blocked
    in
    let login () =
      Service.User.login
        ~ctx:(Pool_database.to_ctx database_label)
        email
        ~password
      >|> handle_result
    in
    suspension_error login blocked
  in
  key |> Cache.find_opt |> CCOption.value ~default:init |> handle_login
;;
