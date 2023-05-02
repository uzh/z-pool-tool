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

let log_request req email =
  let open Opium in
  let open Request in
  let tags = Pool_context.Logger.Tags.req req in
  let ip =
    Headers.get req.headers "X-Real-IP"
    |> CCOption.value ~default:"X-Real-IP not found"
  in
  Logs.info ~src (fun m -> m "Failed login attempt: %s %s" ip email ~tags)
;;

let login req urlencoded database_label =
  let open Utils.Lwt_result.Infix in
  let* email, password = login_params urlencoded in
  let init = 0, None in
  let increment counter =
    let counter = counter + 1 in
    counter, block_until counter
  in
  let key = database_label, email in
  let handle_login (counter, blocked) =
    let handle_result = function
      | Ok user ->
        let () = Cache.remove key in
        Ok user
      | Error err ->
        log_request req email;
        let () = counter |> increment |> Cache.set key in
        err |> Message.handle_sihl_login_error |> CCResult.fail
    in
    let login () =
      Service.User.login
        ~ctx:(Pool_database.to_ctx database_label)
        email
        ~password
      ||> handle_result
    in
    match blocked with
    | Some blocked when Ptime.(is_earlier (Ptime_clock.now ()) ~than:blocked) ->
      Lwt_result.fail (Message.LoginEmailBlocked blocked)
    | None | _ -> login ()
  in
  key |> Cache.find_opt |> CCOption.value ~default:init |> handle_login
;;
