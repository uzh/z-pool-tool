let src = Logs.Src.create "login helper"

module Cache = struct
  open CCCache

  let equal_find_email (l1, e1) (l2, e2) =
    Pool_database.Label.equal l1 l2 && CCString.equal e1 e2
  ;;

  let cache_find_email
    : (Pool_database.Label.t * string, int * Ptime.t option) t
    =
    replacing ~eq:equal_find_email 2048
  ;;

  let clear () = clear cache_find_email
end

let login database_label email ~password =
  let open Utils.Lwt_result.Infix in
  let default = 0, None in
  let increment (counter, blocked) = counter + 1, blocked in
  let add_to_cache =
    CCCache.add Cache.cache_find_email (database_label, email)
  in
  let[@warning "-27"] cb_logger ~in_cache _ _ =
    CCCache.iter
      Cache.cache_find_email
      (fun (database_label, email) (counter, blocked) ->
      Logs.info (fun m -> m "%s" email);
      Logs.info (fun m -> m "%i" counter);
      ())
  in
  let init _ = default in
  let handle_login (counter, blocked) =
    let handle_result = function
      | Ok user ->
        let (_ : bool) = add_to_cache default in
        Ok user
      | Error err ->
        let (_ : bool) = add_to_cache (increment (counter, blocked)) in
        err |> Pool_common.Message.handle_sihl_login_error |> CCResult.fail
    in
    match blocked with
    | Some blocked when Ptime.(is_earlier (Ptime_clock.now ()) ~than:blocked) ->
      Lwt_result.fail Pool_common.Message.LoginEmailBlocked
    | None | _ ->
      Service.User.login
        ~ctx:(Pool_database.to_ctx database_label)
        email
        ~password
      ||> handle_result
  in
  CCCache.(
    with_cache ~cb:cb_logger Cache.cache_find_email init (database_label, email))
  |> handle_login
;;
