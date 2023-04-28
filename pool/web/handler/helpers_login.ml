module Label = Pool_database.Label

let src = Logs.Src.create "login helper"

module Cache = struct
  open Hashtbl

  let tbl : (Label.t * string, int * Ptime.t option) t = create 100

  let set key value =
    Logs.info (fun m -> m "%s" "Set value");
    replace tbl key value
  ;;

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

let login database_label email ~password =
  let open Utils.Lwt_result.Infix in
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
        let () = counter |> increment |> Cache.set key in
        err |> Pool_common.Message.handle_sihl_login_error |> CCResult.fail
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
      Lwt_result.fail Pool_common.Message.LoginEmailBlocked
    | None | _ -> login ()
  in
  key |> Cache.find_opt |> CCOption.value ~default:init |> handle_login
;;
